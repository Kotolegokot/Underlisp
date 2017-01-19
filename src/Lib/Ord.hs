module Lib.Ord (builtinFunctions
               ,specialOperators) where

import Base

default (Int)

biEQ :: [SExpr] -> Eval SExpr
biEQ [arg1, arg2] = return . bool $ arg1 == arg2
biEQ _            = reportUndef "two arguments required"

biLT :: [SExpr] -> Eval SExpr
biLT [arg1, arg2] = return . bool $ arg1 < arg2
biLT _            = reportUndef "two arguments required"

builtinFunctions = [("=", Just 2, biEQ)
                   ,("<", Just 2, biLT)]

specialOperators = []
