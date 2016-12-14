module Lib.IO (builtin_print,
               builtin_print_ln,
               builtin_flush,
               builtin_get_line) where 

import System.IO (stdout, hFlush)
import SExpr
import Lib.Internal

builtin_print :: [SExpr] -> IO SExpr
builtin_print sexprs = mapM (putStr . show_sexpr) sexprs >> return empty_list

builtin_print_ln :: [SExpr] -> IO SExpr
builtin_print_ln sexprs = mapM (putStrLn . show_sexpr) sexprs >> return empty_list

builtin_flush :: [SExpr] -> IO SExpr
builtin_flush [] = hFlush stdout >> return empty_list
builtin_flush _  = error "'flush' requires no arguments"

builtin_get_line :: [SExpr] -> IO SExpr
builtin_get_line [] = getLine >>= (return . SString)
builtin_get_line _  = error "'get-line' requires no arguments"
