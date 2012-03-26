module Control.Pipe.PutbackPipe where

import Control.Pipe
import Control.Pipe.Combinators
import Control.Pipe.Monoidal

newtype PutbackPipe a b m r = PutbackPipe {
    unPutback :: Pipe a (Either b a) m r
    }

instance (Monad m) => Monad (PutbackPipe a b m) where
    return = PutbackPipe . return
    (PutbackPipe p) >>= f = PutbackPipe (p >>= unPutback . f)

nonputback :: (Monad m) => Pipe a b m r -> PutbackPipe a b m r
nonputback p = PutbackPipe (p >+> pipe Left)

putback :: (Monad m) => a -> PutbackPipe a b m ()
putback = PutbackPipe . yield . Right

yieldPB :: (Monad m) => b -> PutbackPipe a b m ()
yieldPB = PutbackPipe . yield . Left

awaitPB  :: (Monad m) => PutbackPipe a b m a
awaitPB  = PutbackPipe await

tryAwaitPB  :: (Monad m) => PutbackPipe a b m (Maybe a)
tryAwaitPB  = PutbackPipe tryAwait

runPutback :: (Monad m) => PutbackPipe a b m r -> Pipe a b m r
runPutback pb = loopP (joinP >+> (unPutback pb))
        
