module Util (shit
            , bind_args) where

import qualified Data.Map as Map
import Data.Map (Map)
import Expr
import Callable
import Point

shit :: Point -> String -> a
shit Undefined                   msg = error msg
shit (Point filename row column) msg = error $ prefix ++ msg
  where prefix = filename ++ ":" ++ show column ++ ":" ++ show row ++ ": error: "

bind_args :: Expr e a => Prototype -> [a] -> Map String a
bind_args (Prototype arg_names False) args
  | length arg_names > length args = error "too little arguments"
  | length arg_names < length args = error "too many arguments"
  | otherwise                      = Map.fromList (zip arg_names args)
bind_args (Prototype arg_names True) args
  | length arg_names - 1 > length args = error "too little arguments"
  | otherwise                          = let (left, right) = splitAt (length arg_names - 1) args
                                             args'         = left ++ [list right]
                                         in Map.fromList (zip arg_names args')
