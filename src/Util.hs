module Util where

import Data.IORef
import Data.Maybe (fromMaybe)
import Control.Spoon (teaspoon)
import Control.Monad.State

-- handy functions for list state monads
getHead :: MonadState [s] m => m s
getHead = do
  (x:xs) <- get
  put xs
  return x

putHead :: MonadState [s] m => s -> m ()
putHead x = do
  xs <- get
  put (x:xs)

empty :: MonadState [s] m => m Bool
empty = do
  xs <- get
  put xs
  return $ null xs

-- | like teaspoon, but returns default value in the case of Nothing
fromFalsum :: a -> a -> a
fromFalsum def exp = fromMaybe def (teaspoon exp)

-- | monadic variant of 'fromFalsum'
fromFalsumM :: Monad m => m a -> a -> m a
fromFalsumM def exp = case teaspoon exp of
  Just val -> return val
  Nothing  -> def

-- some functions to work with mutable references
exploreIORef :: IORef a -> (a -> b) -> IO b
exploreIORef ref f = f <$> readIORef ref

exploreIORefIO :: IORef a -> (a -> IO b) -> IO b
exploreIORefIO ref f = f =<< readIORef ref

modifyIORefIO :: IORef a -> (a -> IO a) -> IO ()
modifyIORefIO ref f = do
  a <- readIORef ref
  a' <- f a
  writeIORef ref a'

modifyIORefIO' :: IORef a -> (a -> IO a) -> IO ()
modifyIORefIO' ref f = do
  a <- readIORef ref
  a' <- f a
  writeIORef ref (seq a' a')
