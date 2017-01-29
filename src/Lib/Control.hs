module Lib.Control (builtinFunctions
                   ,specialOperators) where

import Control.Conditional (if')
import Data.IORef
import Base
import Evaluator

default (Int)

soIf :: IORef Scope -> [SExpr] -> EvalM SExpr
soIf scopeRef [condExp]                    = soIf scopeRef [condExp, nil,     nil]
soIf scopeRef [condExp, trueExp]           = soIf scopeRef [condExp, trueExp, nil]
soIf scopeRef [condExp, trueExp, falseExp] = do
  cond <- getBool =<< evalAlone scopeRef condExp
  eval scopeRef $ if' cond trueExp falseExp
soIf _ _                                   = reportE' "1 to 3 arguments requried"

soScope :: IORef Scope -> [SExpr] -> EvalM SExpr
soScope = evalBody

soSeq :: IORef Scope -> [SExpr] -> EvalM SExpr
soSeq scopeRef exps = do
  result <- evalSeq scopeRef exps
  return $ if null result then nil else last result

builtinFunctions = []

specialOperators = [("if",    Just 3,  soIf)
                   ,("scope", Nothing, soScope)
                   ,("seq",   Nothing, soSeq)]

