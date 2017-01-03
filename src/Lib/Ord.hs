module Lib.Ord (builtinFunctions
               ,specialOperators) where

import Base
import Exception

biEQ :: [SExpr] -> IO SExpr
biEQ [arg1, arg2] = return . bool $ arg1 == arg2
biEQ _            = reportUndef "two arguments required"

biLT :: [SExpr] -> IO SExpr
biLT [arg1, arg2] = return . bool $ arg1 < arg2
biLT _            = reportUndef "two arguments required"

builtinFunctions = [("=", Just (2 :: Int), biEQ)
                   ,("<", Just 2,          biLT)]

specialOperators = []
