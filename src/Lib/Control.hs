{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Control (spop_if,
                    spop_seq) where

import SExpr
import qualified Env
import LexicalEnvironment
import Callable
import Exception

spop_if :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_if eval eval_scope e [cond_sexpr]                          = spop_if eval eval_scope e [cond_sexpr, nil,        nil]
spop_if eval eval_scope e [cond_sexpr, true_sexpr]              = spop_if eval eval_scope e [cond_sexpr, true_sexpr, nil]
spop_if eval eval_scope e [cond_sexpr, true_sexpr, false_sexpr] = do
    (_, cond) <- eval e cond_sexpr
    if from_bool cond
      then do
        (_, expr) <- eval e true_sexpr
        return (e, expr)
      else do
        (_, expr) <- eval e false_sexpr
        return (e, expr)
spop_if _    _          _        _                                    = error "if: 1 to 3 arguments requried"

spop_seq :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_seq _ eval_scope context args = do
  (_, expr) <- eval_scope context args
  return (context, expr)
