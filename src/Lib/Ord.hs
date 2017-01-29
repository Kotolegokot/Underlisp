module Lib.Ord (builtinFunctions
               ,specialOperators) where

import Data.IORef
import Base

default (Int)

biEQ :: IORef Scope -> [SExpr] -> EvalM SExpr
biEQ _ [arg1, arg2] = return . bool $ arg1 == arg2
biEQ _ _            = reportE' "two arguments required"

biLT :: IORef Scope -> [SExpr] -> EvalM SExpr
biLT _ [arg1, arg2] = return . bool $ arg1 < arg2
biLT _ _            = reportE' "two arguments required"

builtinFunctions = [("=", Just 2, biEQ)
                   ,("<", Just 2, biLT)]

specialOperators = []
