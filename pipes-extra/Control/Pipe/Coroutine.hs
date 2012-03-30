{-# LANGUAGE DeriveDataTypeable #-}
module Control.Pipe.Coroutine (
  Coroutine,
  coroutine,
  suspend,
  suspendE,
  resume,
  step,
  terminate,
  resumeWith,
  stepWith,
  resumeBoth,
  stepBoth
  ) where

import Control.Monad
import Control.Pipe
import Control.Pipe.Exception
import qualified Control.Exception as E
import Data.Typeable
import Prelude hiding (catch)

data Coroutine a b m r = Coroutine
  { suspend :: Pipe a b m r
  , suspendE :: E.SomeException -> Pipe a b m r }

resume :: Monad m
       => Pipe a b m r
       -> Pipe a x m (Either r (b, Coroutine a b m r))
resume (Pure r) = return $ Left r
resume (Throw e) = throwP e
resume (Free c h) = go c >>= \x -> case x of
  Left p       -> resume p
  Right (b, p) -> return $ Right (b, Coroutine p h)
  where
    go (Await k) = liftM (Left . k) await
    go (Yield b p) = return $ Right (b, p)
    go (M m s) = liftM Left $ liftP s m

coroutine :: Monad m
          => Pipe a b m r
          -> Coroutine a b m r
coroutine p = Coroutine p throwP

step :: Monad m
     => Coroutine a b m r
     -> Pipe a x m (Either r (b, Coroutine a b m r))
step = resume . suspend

data CoroutineTerminated = CoroutineTerminated
  deriving (Show, Typeable)

instance E.Exception CoroutineTerminated

terminate :: Monad m
          => Coroutine a b m r
          -> Pipe a b m ()
terminate p = go (suspendE p (E.toException CoroutineTerminated))
  where
    go (Pure r) = return ()
    go (Throw e) = return ()
    go (Free c h) = catchP (step c) (return . h) >>= go

    step (Await k) = liftM k await
    step (Yield b p) = return p
    step (M m (Finalizer _)) = ensure m
    step (M m s) = liftP s m


resumeWith :: (Monad m)
    => (Maybe a)
    -> Pipe a b m r
    -> Pipe x b m (Either (Maybe a,r) (Coroutine a b m r))
resumeWith ma c = step ma c where
    step v (Pure r) = return (Left (v,r))
    step _ (Throw e) = throwP e
    step v@(Just a) p@(Free c h) = go c where
        go (Await k) = step Nothing (k a)
        go (Yield b p) = yield b >> step v p
        go (M m s) = liftP s m >>= step v
    step v@Nothing p@(Free c h) = go c where
        go (Await k) = return (Right (Coroutine p h))
        go (Yield b p') = yield b >> step v p'
        go (M m s) = liftP s m >>= step v

stepWith :: (Monad m)
    => Maybe a
    -> Coroutine a b m r
    -> Pipe x b m (Either (Maybe a,r) (Coroutine a b m r))
stepWith ma = resumeWith ma . suspend

resumeBoth :: (Monad m)
    => (Maybe a)
    -> Pipe a b m r
    -> Pipe x y m (Maybe a, Either r (Maybe b,Coroutine a b m r))
resumeBoth ma c = step ma c where
    step v (Pure r) = return (v,Left r)
    step _ (Throw e) = throwP e
    step v@(Just a) p@(Free c h) = go c where
        go (Await k) = step Nothing (k a)
        go (Yield b p') = return (v,Right (Just b,Coroutine p' h))
        go (M m s) = liftP s m >>= step v
    step v@Nothing p@(Free c h) = go c where
        go (Await k) = return (v,Right (Nothing,Coroutine p h))
        go (Yield b p') = return (v,Right (Just b,Coroutine p' h))
        go (M m s) = liftP s m >>= step v

stepBoth :: (Monad m)
    => Maybe a
    -> Coroutine a b m r
    -> Pipe x y m (Maybe a, Either r (Maybe b,Coroutine a b m r))
stepBoth ma = resumeBoth ma . suspend

