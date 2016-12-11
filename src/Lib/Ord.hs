module Lib.Ord (builtin_eq,
                builtin_ne,
                builtin_lt,
                builtin_gt,
                builtin_le,
                builtin_ge) where

import Expr
import Lib.Internal

builtin_eq :: [SExpr] -> IO SExpr
builtin_eq [arg1, arg2] = return . SBool $ arg1 == arg2
builtin_eq _            = error "=: two arguments required"

builtin_ne :: [SExpr] -> IO SExpr
builtin_ne [arg1, arg2] = return . SBool $ arg1 /= arg2
builtin_ne _            = error "/=: two arguments required"

builtin_lt :: [SExpr] -> IO SExpr
builtin_lt [arg1, arg2] = return . SBool $ arg1 < arg2
builtin_lt _            = error "<: two arguments required"

builtin_gt :: [SExpr] -> IO SExpr
builtin_gt [arg1, arg2] = return . SBool $ arg1 > arg2
builtin_gt _            = error ">: two arguments required"

builtin_le :: [SExpr] -> IO SExpr
builtin_le [arg1, arg2] = return . SBool $ arg1 <= arg2
builtin_le _            = error "<=: two arguments required"

builtin_ge :: [SExpr] -> IO SExpr
builtin_ge [arg1, arg2] = return . SBool $ arg1 >= arg2
builtin_ge _            = error ">=: two arguments required"
