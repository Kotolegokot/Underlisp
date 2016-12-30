module Lib.IO (builtinPutChar
              , builtinToString
              , builtinFlush
              , builtinGetLine) where

import System.IO (stdout, hFlush)
import SExpr
import LispShow
import Exception

builtinFlush :: [SExpr] -> IO SExpr
builtinFlush [] = hFlush stdout >> return nil
builtinFlush _  = reportUndef "no arguments required"

builtinGetLine :: [SExpr] -> IO SExpr
builtinGetLine [] = getLine >>= (return . list . map char)
builtinGetLine _  = reportUndef "no arguments required"

builtinToString :: [SExpr] -> IO SExpr
builtinToString [arg] = return . list . map char $ lispShow arg
builtinToString _     = reportUndef "just one argument required"

builtinPutChar :: [SExpr] -> IO SExpr
builtinPutChar [SAtom _ (AChar c)]    = putChar c >> return nil
builtinPutChar [expr]                 = report (point expr) "char expected"
builtinPutChar _                      = reportUndef "just one argument required"
