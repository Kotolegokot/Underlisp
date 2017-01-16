module SWriterT (SWriterT
                ,add
                ,runSWriterT
                ,evalSWriterT
                ,execSWriterT
                ,module Control.Monad.State) where

import Control.Monad.State

type SWriterT w m = StateT [w] m

add :: MonadState [w] m => w -> m ()
add w = do
  ws <- get
  put (w:ws)
  return ()

runSWriterT :: SWriterT w m a -> m (a, [w])
runSWriterT = flip runStateT $ []

evalSWriterT :: Monad m => SWriterT w m a -> m a
evalSWriterT = flip evalStateT $ []

execSWriterT :: Monad m => SWriterT w m a -> m [w]
execSWriterT = flip execStateT $ []
