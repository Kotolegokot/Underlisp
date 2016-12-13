module Lib.Internal (Eval,
                     eval_list,
                     eval_list_with_context,
                     handle_lambda_list) where

import Data.List (elemIndex, elemIndices, delete)
import Control.Monad (foldM)
import SExpr

type Eval = Context -> SExpr -> IO (SExpr, Context)

eval_list :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
eval_list eval context sexprs = do
    (expr, _) <- foldM (\(_, prev_context) sexpr -> eval prev_context sexpr) (empty_list, context) sexprs
    return (expr, context)

eval_list_with_context :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
eval_list_with_context eval context sexprs = foldM (\(_, prev_context) sexpr -> eval prev_context sexpr) (empty_list, context) sexprs

handle_lambda_list :: SExpr -> ([String], Bool)
handle_lambda_list (SList lambda_list)
  | not $ all is_symbol lambda_list = error "every item in lambda list must be a keyword"
  | length ixs > 1                  = error "more than one &rest in lambda list is forbidden"
  | rest && ix /= count - 2         = error "&rest must be last but one"
  | otherwise                       = if rest then (delete "&rest" . map from_symbol $ lambda_list, rest)
                                              else (map from_symbol $ lambda_list, rest)
  where ixs   = elemIndices (SSymbol "&rest") lambda_list
        ix    = head ixs
        rest  = length ixs == 1
        count = length lambda_list
handle_lambda_list' _ = error "lambda list must be a list"
