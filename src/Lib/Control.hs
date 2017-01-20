module Lib.Control (builtinFunctions
                   ,specialOperators) where

import Control.Conditional (if')
import Base
import Evaluator

default (Int)

soIf :: [SExpr] -> Lisp SExpr
soIf [condExp]                    = soIf [condExp, nil,     nil]
soIf [condExp, trueExp]           = soIf [condExp, trueExp, nil]
soIf [condExp, trueExp, falseExp] = do
  cond <- getBool =<< eval condExp
  eval $ if' cond trueExp falseExp
soIf _                            = reportE' "1 to 3 arguments requried"

soScope :: [SExpr] -> Lisp SExpr
soScope = evalScope

soSeq :: [SExpr] -> Lisp SExpr
soSeq = evalScopeInterpolated

builtinFunctions = []

specialOperators = [("if",    Just 3,  soIf)
                   ,("scope", Nothing, soScope)
                   ,("seq",   Nothing, soSeq)]

