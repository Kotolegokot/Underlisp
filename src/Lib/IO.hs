module Lib.IO (builtin_put_char
              , builtin_flush
              , builtin_get_line) where

import System.IO (stdout, hFlush)
import SExpr
import LispShow

builtin_flush :: [SExpr] -> IO SExpr
builtin_flush [] = hFlush stdout >> return nil
builtin_flush _  = error "flush: no arguments required"

builtin_get_line :: [SExpr] -> IO SExpr
builtin_get_line [] = getLine >>= (return . SList . map char)
builtin_get_line _  = error "get-line: no arguments required"

builtin_put_char :: [SExpr] -> IO SExpr
builtin_put_char [SAtom (AChar c)] = putChar c >> return nil
builtin_put_char [_]               = error "put-char: char expected"
builtin_put_char _                 = error "put-char: just one argument required"
