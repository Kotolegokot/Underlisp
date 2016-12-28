module Lib.Ord (builtin_eq,
                builtin_lt) where

import SExpr
import Exception

builtin_eq :: [SExpr] -> IO SExpr
builtin_eq [arg1, arg2] = return . bool $ arg1 == arg2
builtin_eq _            = report_undef "two arguments required"

builtin_lt :: [SExpr] -> IO SExpr
builtin_lt [arg1, arg2] = return . bool $ arg1 < arg2
builtin_lt _            = report_undef "two arguments required"
