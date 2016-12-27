module Util where

import qualified Data.Map as Map
import Data.Map (Map)
import Expr
import Callable

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

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
