module Control.Pipe.PutbackPipe where

import Control.Monad.State
import Control.Monad.Trans
import Control.Applicative

import Control.Pipe
import Control.Pipe.Combinators
import Control.Pipe.Monoidal

newtype PutbackPipe a b m r = PutbackPipe {
    unPutback :: StateT [a] (Pipe a b m) r
    }

-- | FIXME: Move this to pipes-core
instance (MonadIO m) => MonadIO (Pipe a b m)  where
    liftIO = lift . liftIO

instance (Monad m) => Monad (PutbackPipe a b m) where
    return = PutbackPipe . return
    (PutbackPipe p) >>= f = PutbackPipe (p >>= unPutback . f)

instance (Monad m, Functor m) => Functor (PutbackPipe a b m) where
    fmap f (PutbackPipe p) = PutbackPipe (fmap f p)

instance (MonadIO m) => MonadIO (PutbackPipe a b m) where
    liftIO a = PutbackPipe (liftIO a)

nonputback :: (Monad m) => Pipe a b m r -> PutbackPipe a b m r
nonputback p = PutbackPipe (lift p)

putback :: (Monad m) => a -> PutbackPipe a b m ()
putback a = PutbackPipe (modify (a:))

yieldPB :: (Monad m) => b -> PutbackPipe a b m ()
yieldPB = PutbackPipe . lift . yield

awaitPB  :: (Monad m) => PutbackPipe a b m a
awaitPB = PutbackPipe (get >>= decide) where
    decide [] = lift await
    decide (a:as) = put as >> return a

tryAwaitPB  :: (Monad m) => PutbackPipe a b m (Maybe a)
tryAwaitPB = PutbackPipe (get >>= decide) where
    decide [] = lift tryAwait
    decide (a:as) = put as >> return (Just a)

runPutback :: (Monad m) => PutbackPipe a b m r -> Pipe a b m r
runPutback pb = evalStateT (unPutback pb) []

