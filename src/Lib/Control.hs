module Lib.Control (builtinFunctions
                   ,specialOperators) where

import Control.Monad (foldM)
import Base
import Exception

soIf :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soIf eval evalScope e [condSexpr]                        = soIf eval evalScope e [condSexpr, nil,       nil]
soIf eval evalScope e [condSexpr, trueSexpr]             = soIf eval evalScope e [condSexpr, trueSexpr, nil]
soIf eval evalScope e [condSexpr, trueSexpr, falseSexpr] = do
  (_, cond) <- eval e condSexpr
  if fromBool cond
    then do
      (_, expr) <- eval e trueSexpr
      return (e, expr)
    else do
      (_, expr) <- eval e falseSexpr
      return (e, expr)
soIf _    _          _        _                          = reportUndef "1 to 3 arguments requried"

soScope :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soScope _ evalScope e args = do
  (_, expr) <- evalScope e args
  return (e, expr)

soSeq :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soSeq eval _ e = foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e, nil)

builtinFunctions = []

specialOperators = [("if",    Just (3 :: Int), soIf)
                   ,("scope", Nothing,         soScope)
                   ,("seq",   Nothing,         soSeq)]
