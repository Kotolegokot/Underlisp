module Lib.Control (builtin_if,
                    builtin_unless,
                    builtin_seq) where

import Expr
import Lib.Internal

builtin_if :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_if eval context [cond_sexpr]                          = builtin_if eval context [cond_sexpr, empty_list, empty_list]
builtin_if eval context [cond_sexpr, true_sexpr]              = builtin_if eval context [cond_sexpr, true_sexpr, empty_list]
builtin_if eval context [cond_sexpr, true_sexpr, false_sexpr] = do
    (cond, _) <- eval context cond_sexpr
    if from_bool cond
       then eval context true_sexpr
       else eval context false_sexpr
builtin_if _    _       _                                     = error "'if' requires 1 to 3 arguments"

builtin_unless :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_unless eval context [cond_sexpr]                          = builtin_if eval context [cond_sexpr]
builtin_unless eval context [cond_sexpr, false_sexpr]             = builtin_if eval context [cond_sexpr, empty_list, false_sexpr]
builtin_unless eval context [cond_sexpr, false_sexpr, true_sexpr] = builtin_if eval context [cond_sexpr, true_sexpr, false_sexpr]
builtin_unless _    _       _                                     = error "'unless' requires 1 to 3 arguments"

builtin_seq :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_seq eval context args = do
    (expr, _) <- eval_list eval context args
    return (expr, context)
