module Lib.IO (builtin_print,
               builtin_print_ln,
               builtin_flush,
               builtin_get_line) where 

import System.IO (stdout, hFlush)
import Expr
import Lib.Internal

builtin_print :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_print eval context [arg] = do
    (expr, _) <- eval context arg
    putStr $ show_sexpr expr
    return (empty_list, context)
builtin_print _    _       _     = error "'print' requires only one argument"

builtin_print_ln :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_print_ln eval context [arg] = do
    (expr, _) <- eval context arg 
    putStrLn $ show_sexpr expr
    return (empty_list, context)
builtin_print_ln _    _       _     = error "'print-ln' requires only one argument"

builtin_flush :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_flush eval context [] = hFlush stdout >> return (empty_list, context)
builtin_flush _    _       _  = error "'flush' requires no arguments"

builtin_get_line :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_get_line eval context [] = do
    line <- getLine
    return (SString line, context)
builtin_get_line _    _       _  = error "'get-line' requires no arguments"
