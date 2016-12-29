module Lib.IO (builtinPutChar
              , builtinWrite
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

builtinWrite :: [SExpr] -> IO SExpr
builtinWrite [arg] = putStr (lispShow arg) >> return nil
builtinWrite []    = reportUndef "just one argument required"

builtinPutChar :: [SExpr] -> IO SExpr
builtinPutChar [SAtom _ (AChar c)]    = putChar c >> return nil
builtinPutChar [expr]                 = report (point expr) "char expected"
builtinPutChar _                      = reportUndef "just one argument required"
