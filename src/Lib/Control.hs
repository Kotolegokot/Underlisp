module Lib.Control (spop_if,
                    spop_unless,
                    spop_seq) where

import SExpr
import Lib.Internal

spop_if :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_if eval context [cond_sexpr]                          = spop_if eval context [cond_sexpr, empty_list, empty_list]
spop_if eval context [cond_sexpr, true_sexpr]              = spop_if eval context [cond_sexpr, true_sexpr, empty_list]
spop_if eval context [cond_sexpr, true_sexpr, false_sexpr] = do
    (cond, _) <- eval context cond_sexpr
    if from_bool cond
       then eval context true_sexpr
       else eval context false_sexpr
spop_if _    _       _                                     = error "'if' requires 1 to 3 arguments"

spop_unless :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_unless eval context [cond_sexpr]                          = spop_if eval context [cond_sexpr]
spop_unless eval context [cond_sexpr, false_sexpr]             = spop_if eval context [cond_sexpr, empty_list, false_sexpr]
spop_unless eval context [cond_sexpr, false_sexpr, true_sexpr] = spop_if eval context [cond_sexpr, true_sexpr, false_sexpr]
spop_unless _    _       _                                     = error "'unless' requires 1 to 3 arguments"

spop_seq :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_seq eval context args = do
    (expr, _) <- eval_list eval context args
    return (expr, context)
