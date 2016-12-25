module Lib.IO (builtin_print,
               builtin_print_ln,
               builtin_flush,
               builtin_get_line) where

import System.IO (stdout, hFlush)
import SExpr
import LispShow

builtin_print :: [SExpr] -> IO SExpr
builtin_print sexprs = mapM (putStr . lisp_show) sexprs >> return nil

builtin_print_ln :: [SExpr] -> IO SExpr
builtin_print_ln sexprs = mapM (putStr . lisp_show) sexprs >> putStrLn "" >> return nil

builtin_flush :: [SExpr] -> IO SExpr
builtin_flush [] = hFlush stdout >> return nil
builtin_flush _  = error "'flush' requires no arguments"

builtin_get_line :: [SExpr] -> IO SExpr
builtin_get_line [] = getLine >>= (return . SList . map char)
builtin_get_line _  = error "'get-line' requires no arguments"
