{-# LANGUAGE FlexibleContexts #-}
module SWriterT where

import Control.Monad.State

type SWriterT w m = StateT [w] m

add :: MonadState [w] m => w -> m ()
add w = do
  ws <- get
  put (w:ws)
  return ()

runSWriterT :: SWriterT w m a -> m (a, [w])
runSWriterT = flip runStateT $ []
