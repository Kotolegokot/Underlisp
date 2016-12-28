module Util where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (elemIndices, delete)
import Expr
import SExpr
import Prototype
import Exception

bind_args :: Expr e a => Prototype -> [a] -> Map String a
bind_args (Prototype arg_names False) args
  | length arg_names > length args = report_undef "too little arguments"
  | length arg_names < length args = report_undef "too many arguments"
  | otherwise                      = Map.fromList (zip arg_names args)
bind_args (Prototype arg_names True) args
  | length arg_names - 1 > length args = report_undef "too little arguments"
  | otherwise                          = let (left, right) = splitAt (length arg_names - 1) args
                                             args'         = left ++ [list right]
                                         in Map.fromList (zip arg_names args')

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- | takes an s-list of the form (arg1 arg2... [&rst argLast])
-- | and constructs a Prototype
parse_lambda_list :: SExpr -> Prototype
parse_lambda_list (SList p lambda_list)
  | not $ all is_symbol lambda_list = report p "all items in a lambda list must be symbols"
  | length ixs > 1                  = report p "more than one &rest in a lambda list is forbidden"
  | rest && ix /= count - 2         = report p "&rest must be last but one"
  | otherwise                       = if rest
                                      then Prototype (delete "&rest" . map from_symbol $ lambda_list) rest
                                      else Prototype (map from_symbol $ lambda_list) rest
  where ixs   = elemIndices (symbol "&rest") lambda_list
        ix    = head ixs
        rest  = length ixs == 1
        count = length lambda_list
parse_lambda_list _ = report_undef "lambda list must be a list"
