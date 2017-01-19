module Lib.Control (builtinFunctions
                   ,specialOperators) where

import Control.Conditional (if')
import Control.Arrow (first)
import Control.Monad (foldM)
import Base
import Evaluator

default (Int)

soIf :: Env -> [SExpr] -> Eval (Env, SExpr)
soIf e [condExp]                    = soIf e [condExp, nil,     nil]
soIf e [condExp, trueExp]           = soIf e [condExp, trueExp, nil]
soIf e [condExp, trueExp, falseExp] = do
  (_, cond) <- eval e condExp
  cond' <- getBool cond
  first (const e) <$> eval e (if' cond' trueExp falseExp)
soIf _        _                          = reportUndef "1 to 3 arguments requried"

soScope :: Env -> [SExpr] -> Eval (Env, SExpr)
soScope e args = first (const e) <$> evalScope e args

soSeq :: Env -> [SExpr] -> Eval (Env, SExpr)
soSeq e = foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e, nil)

builtinFunctions = []

specialOperators = [("if",    Just 3,  soIf)
                   ,("scope", Nothing, soScope)
                   ,("seq",   Nothing, soSeq)]

