module LispState where

import Control.Monad.State
import Point

data Call = Call { cPoint  :: Point
                 , cCaller :: String }

type LispState = StateT [Call] IO

push :: Call -> LispState ()
push x = do
  xs <- get
  put (x:xs)
  return ()

pop :: LispState Call
pop = do
  (x:xs) <- get
  put xs
  return x

empty :: LispState Bool
empty = do
  a <- get
  put a
  return (null a)

io :: IO a -> LispState a
io = liftIO
