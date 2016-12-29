module Util where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (elemIndices, delete)
import Expr
import SExpr
import Prototype
import Exception

bindArgs :: Expr e a => Prototype -> [a] -> Map String a
bindArgs (Prototype argNames False) args
  | length argNames > length args = reportUndef "too little arguments"
  | length argNames < length args = reportUndef "too many arguments"
  | otherwise                      = Map.fromList (zip argNames args)
bindArgs (Prototype argNames True) args
  | length argNames - 1 > length args = reportUndef "too little arguments"
  | otherwise                         = let (left, right) = splitAt (length argNames - 1) args
                                            args'         = left ++ [list right]
                                        in Map.fromList (zip argNames args')

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- | takes an s-list of the form (arg1 arg2... [&rst argLast])
-- | and constructs a Prototype
parseLambdaList :: SExpr -> Prototype
parseLambdaList (SList p lambdaList)
  | not $ all isSymbol lambdaList  = report p "all items in a lambda list must be symbols"
  | length ixs > 1                 = report p "more than one &rest in a lambda list is forbidden"
  | rest && ix /= count - 2        = report p "&rest must be last but one"
  | otherwise                      = if rest
                                     then Prototype (delete "&rest" . map fromSymbol $ lambdaList) rest
                                     else Prototype (map fromSymbol $ lambdaList) rest
  where ixs   = elemIndices (symbol "&rest") lambdaList
        ix    = head ixs
        rest  = length ixs == 1
        count = length lambdaList
parseLambdaList _ = reportUndef "lambda list must be a list"
