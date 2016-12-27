module Lib.IO (builtin_put_char
              , builtin_write
              , builtin_flush
              , builtin_get_line) where

import System.IO (stdout, hFlush)
import SExpr
import LispShow
import Exception

builtin_flush :: [SExpr] -> IO SExpr
builtin_flush [] = hFlush stdout >> return nil
builtin_flush _  = report_undef "flush: no arguments required"

builtin_get_line :: [SExpr] -> IO SExpr
builtin_get_line [] = getLine >>= (return . list . map char)
builtin_get_line _  = report_undef "get-line: no arguments required"

builtin_write :: [SExpr] -> IO SExpr
builtin_write [arg] = putStr (lisp_show arg) >> return nil
builtin_write []    = report_undef "write: just one argument required"

builtin_put_char :: [SExpr] -> IO SExpr
builtin_put_char [SAtom _ (AChar c)]    = putChar c >> return nil
builtin_put_char [expr]                 = report (point expr) "put-char: char expected"
builtin_put_char _                      = report_undef "put-char: just one argument required"
