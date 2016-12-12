module Lib.Boolean (builtin_not,
                    spop_and,
                    spop_or,
                    spop_impl) where

import SExpr
import Lib.Internal

builtin_not :: [SExpr] -> IO SExpr
builtin_not [sexpr] = return . SBool . not . from_bool $ sexpr
builtin_not _       = error "'not' requires just one argument"

spop_and :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_and eval context (x:xs) = do
    (expr, _) <- eval context x
    case from_bool expr of
           True  -> spop_and eval context xs
           False -> return (SBool False, context)
spop_and _    context []     = return (SBool True, context)

spop_or :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_or eval context (x:xs) = do
    (expr, _) <- eval context x
    case from_bool expr of
      True  -> return (SBool True, context)
      False -> spop_or eval context xs
spop_or _    context []     = return (SBool False, context)

spop_impl :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_impl eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    if not $ from_bool expr1
       then return (SBool True, context)
       else eval context arg2
spop_impl _    _       _            = error "'->' requires two arguments"
