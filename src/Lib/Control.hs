{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Control (spopIf,
                    spopSeq) where

import SExpr
import qualified Env
import LexicalEnvironment
import Callable
import Exception

spopIf :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopIf eval evalScope e [condSexpr]                        = spopIf eval evalScope e [condSexpr, nil,       nil]
spopIf eval evalScope e [condSexpr, trueSexpr]             = spopIf eval evalScope e [condSexpr, trueSexpr, nil]
spopIf eval evalScope e [condSexpr, trueSexpr, falseSexpr] = do
    (_, cond) <- eval e condSexpr
    if fromBool cond
      then do
        (_, expr) <- eval e trueSexpr
        return (e, expr)
      else do
        (_, expr) <- eval e falseSexpr
        return (e, expr)
spopIf _    _          _        _                                    = reportUndef "1 to 3 arguments requried"

spopSeq :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopSeq _ evalScope e args = do
  (_, expr) <- evalScope e args
  return (e, expr)
