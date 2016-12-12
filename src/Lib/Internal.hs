module Lib.Internal (Eval,
                     eval_list) where

import Control.Monad (foldM)
import SExpr

type Eval = Context -> SExpr -> IO (SExpr, Context)

eval_list :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
eval_list eval context sexprs = do
    (expr, _) <- foldM (\(_, prev_context) sexpr -> eval prev_context sexpr) (empty_list, context) sexprs
    return (expr, context)
