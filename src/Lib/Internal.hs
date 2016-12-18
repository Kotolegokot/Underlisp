module Lib.Internal (Eval, EvalScope) where

import SExpr

type Eval      = Context -> SExpr   -> IO (Context, SExpr)
type EvalScope = Context -> [SExpr] -> IO (Context, SExpr)
