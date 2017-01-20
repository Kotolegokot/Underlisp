module Lib.Ord (builtinFunctions
               ,specialOperators) where

import Base

default (Int)

biEQ :: [SExpr] -> Lisp SExpr
biEQ [arg1, arg2] = return . bool $ arg1 == arg2
biEQ _            = reportE' "two arguments required"

biLT :: [SExpr] -> Lisp SExpr
biLT [arg1, arg2] = return . bool $ arg1 < arg2
biLT _            = reportE' "two arguments required"

builtinFunctions = [("=", Just 2, biEQ)
                   ,("<", Just 2, biLT)]

specialOperators = []
