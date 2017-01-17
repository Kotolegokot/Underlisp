module Lib.Control (builtinFunctions
                   ,specialOperators) where

import Control.Monad (foldM)
import Base
import Evaluator

soIf :: Env -> [SExpr] -> Eval (Env, SExpr)
soIf e [condSexpr]                        = soIf e [condSexpr, nil,       nil]
soIf e [condSexpr, trueSexpr]             = soIf e [condSexpr, trueSexpr, nil]
soIf e [condSexpr, trueSexpr, falseSexpr] = do
  (_, cond) <- eval e condSexpr
  if fromBool cond
    then do
      (_, expr) <- eval e trueSexpr
      return (e, expr)
    else do
      (_, expr) <- eval e falseSexpr
      return (e, expr)
soIf _        _                          = reportUndef "1 to 3 arguments requried"

soScope :: Env -> [SExpr] -> Eval (Env, SExpr)
soScope e args = do
  (_, expr) <- evalScope e args
  return (e, expr)

soSeq :: Env -> [SExpr] -> Eval (Env, SExpr)
soSeq e = foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e, nil)

builtinFunctions = []

specialOperators = [("if",    Just (3 :: Int), soIf)
                   ,("scope", Nothing,         soScope)
                   ,("seq",   Nothing,         soSeq)]
