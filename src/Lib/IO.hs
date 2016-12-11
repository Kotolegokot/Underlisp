module Lib.IO (builtin_print,
               builtin_print_ln,
               builtin_flush,
               builtin_get_line) where 

import System.IO (stdout, hFlush)
import Expr
import Lib.Internal

builtin_print :: [SExpr] -> IO SExpr
builtin_print [sexpr] = (putStr . show_sexpr $ sexpr) >> return empty_list
builtin_print _       = error "'print' requires just one argument"

builtin_print_ln :: [SExpr] -> IO SExpr
builtin_print_ln [sexpr] = (putStrLn . show_sexpr $ sexpr) >> return empty_list
builtin_print_ln _       = error "'print-ln' requires just one argument"

builtin_flush :: [SExpr] -> IO SExpr
builtin_flush [] = hFlush stdout >> return empty_list
builtin_flush _  = error "'flush' requires no arguments"

builtin_get_line :: [SExpr] -> IO SExpr
builtin_get_line [] = getLine >>= (return . SString)
builtin_get_line _  = error "'get-line' requires no arguments"
