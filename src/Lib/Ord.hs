module Lib.Ord (builtin_eq,
                builtin_ne,
                builtin_lt,
                builtin_gt,
                builtin_le,
                builtin_ge) where

import Expr
import Lib.Internal

builtin_eq :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_eq eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 == expr2, context)
builtin_eq _    _       _            = error "'=' requires two arguments"

builtin_ne :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_ne eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 /= expr2, context)
builtin_ne _      _       _          = error "'/=' requires two arguments"

builtin_lt :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_lt eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 < expr2, context)
builtin_lt _    _       _            = error "'<' requires two arguments"

builtin_gt :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_gt eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 > expr2, context)


builtin_le :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_le eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 <= expr2, context)
builtin_le _    _       _            = error "'<=' requires two arguments"

builtin_ge :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_ge eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 >= expr2, context)
builtin_ge _    _       _            = error "'>=' requires two arguments"
