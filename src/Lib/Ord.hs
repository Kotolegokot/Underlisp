module Lib.Ord (builtinEQ,
                builtinLT) where

import SExpr
import Exception

builtinEQ :: [SExpr] -> IO SExpr
builtinEQ [arg1, arg2] = return . bool $ arg1 == arg2
builtinEQ _            = reportUndef "two arguments required"

builtinLT :: [SExpr] -> IO SExpr
builtinLT [arg1, arg2] = return . bool $ arg1 < arg2
builtinLT _            = reportUndef "two arguments required"
