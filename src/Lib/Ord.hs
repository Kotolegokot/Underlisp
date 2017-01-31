module Lib.Ord (specialOperators) where

import Data.IORef
import Base
import Evaluator

default (Int)

biEQ :: IORef Scope -> [SExpr] -> EvalM SExpr
biEQ _ [arg1, arg2] = return . bool $ arg1 == arg2
biEQ _ _            = reportE' "two arguments required"

biLT :: IORef Scope -> [SExpr] -> EvalM SExpr
biLT _ [arg1, arg2] = return . bool $ arg1 < arg2
biLT _ _            = reportE' "two arguments required"

specialOperators = [("=", Just 2, withEvaluatedArgs biEQ)
                   ,("<", Just 2, withEvaluatedArgs biLT)]
