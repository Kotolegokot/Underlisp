module Lib.Internal (Eval, EvalScope) where

import SExpr
import qualified Env
import Env (Env)

type Eval      = Env SExpr -> SExpr   -> IO (Env SExpr, SExpr)
type EvalScope = Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
