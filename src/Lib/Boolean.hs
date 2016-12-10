module Lib.Boolean (builtin_not,
                    builtin_and,
                    builtin_or,
                    builtin_impl) where

import SExpr
import Lib.Internal

builtin_not :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_not eval context [arg] = do
    (expr, _) <- eval context arg
    return (SBool . not . from_bool $ expr, context)
builtin_not _    _       _     = error "'not' requires just one argument"

builtin_and :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_and eval context (x:xs) = do
    (expr, _) <- eval context x
    case from_bool expr of
           True  -> builtin_and eval context xs
           False -> return (SBool False, context)
builtin_and _    context []     = return (SBool True, context)

builtin_or :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_or eval context (x:xs) = do
    (expr, _) <- eval context x
    case from_bool expr of
      True  -> return (SBool True, context)
      False -> builtin_or eval context xs
builtin_or _    context []     = return (SBool False, context)

builtin_impl :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_impl eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    if not $ from_bool expr1
       then return (SBool True, context)
       else eval context arg2
builtin_impl _    _       _            = error "'->' requires two arguments"
