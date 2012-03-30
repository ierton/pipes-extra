{-# LANGUAGE DeriveDataTypeable #-}
module Control.Pipe.Coroutine (
  Coroutine,
  resume,
  suspend,
  coroutine,
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
  { resume :: Pipe a b m r
  , finalizer :: [m ()]
  }

suspend :: Monad m
        => Pipe a b m r
        -> Pipe a x m (Either r (b, Coroutine a b m r))
suspend (Pure r w) = Pure (Left r) w
suspend (Throw e w) = Throw e w
suspend (Yield x p w) = return (Right (x, Coroutine p w))
suspend (M s m h) = M s (liftM suspend m) (suspend . h)
suspend (Await k h) = Await (suspend . k) (suspend . h)

coroutine :: Monad m
          => Pipe a b m r
          -> Coroutine a b m r
coroutine p = Coroutine p []

step :: Monad m
     => Coroutine a b m r
     -> Pipe a x m (Either r (b, Coroutine a b m r))
step = suspend . resume

terminate :: Monad m
          => Coroutine a b m r
          -> Pipe a b m ()
terminate p = mapM_ masked (finalizer p)

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

