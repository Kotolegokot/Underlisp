module Util (shit
            , Point (..)
            , forward_row
            , forward_column
            , start_point
            , bind_args) where

import qualified Data.Map as Map
import Data.Map (Map)
import Expr
import Callable

shit :: Point -> String -> a
shit Undefined                   msg = error msg
shit (Point filename row column) msg = error $ prefix ++ msg
  where prefix = filename ++ ":" ++ show column ++ ":" ++ show row ++ ": error: "

data Point = Point { filename :: String
                   , row :: Int
                   , column :: Int }
           | Undefined
  deriving (Eq, Show)

forward_row :: Point -> Point
forward_row Undefined = Undefined
forward_row point = point { row = row point + 1, column = 0 }

forward_column :: Point -> Point
forward_column Undefined = Undefined
forward_column point = point { column = column point + 1 }

start_point :: String -> Point
start_point filename = Point filename 0 0

bind_args :: Expr e a => Prototype -> [a] -> Map String a
bind_args (Prototype arg_names False) args
  | length arg_names > length args = error "too little arguments"
  | length arg_names < length args = error "too many arguments"
  | otherwise                      = foldl (\e (name, value) -> Map.insert name value e) Map.empty (zip arg_names args)
bind_arg (Prototype arg_names True) args
  | length arg_names - 1 > length args = error "too little arguments"
  | otherwise                          = let (left, right) = splitAt (length arg_names - 1) args
                                             args'         = left ++ [list right]
                                         in  foldl (\e (name, value) -> Map.insert name value e) Map.empty (zip arg_names args')
