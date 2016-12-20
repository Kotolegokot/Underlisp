module Lib.Control (spop_if,
                    spop_seq) where

import SExpr
import qualified Env
import Env (Env)
import Lib.Internal

spop_if :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_if eval eval_scope context [cond_sexpr]                          = spop_if eval eval_scope context [cond_sexpr, nil,        nil]
spop_if eval eval_scope context [cond_sexpr, true_sexpr]              = spop_if eval eval_scope context [cond_sexpr, true_sexpr, nil]
spop_if eval eval_scope context [cond_sexpr, true_sexpr, false_sexpr] = do
    (_, cond) <- eval context cond_sexpr
    if from_bool cond
       then eval context true_sexpr
       else eval context false_sexpr
spop_if _    _          _        _                                    = error "if: 1 to 3 arguments requried"

spop_seq :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_seq _ eval_scope context args = do
  (_, expr) <- eval_scope context args
  return (context, expr)
