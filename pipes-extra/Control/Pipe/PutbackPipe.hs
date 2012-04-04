module Control.Pipe.PutbackPipe where

import Control.Monad.State
import Control.Monad.Trans

import Control.Pipe
import Control.Pipe.Combinators
import Control.Pipe.Monoidal

newtype PutbackPipe a b m r = PutbackPipe {
    unPutback :: StateT [a] (Pipe a b m) r
    }

instance (Monad m) => Monad (PutbackPipe a b m) where
    return = PutbackPipe . return
    (PutbackPipe p) >>= f = PutbackPipe (p >>= unPutback . f)

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

