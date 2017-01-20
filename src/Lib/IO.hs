module Lib.IO (builtinFunctions
              ,specialOperators) where

import Control.Monad.IO.Class (liftIO)

import System.IO (stdout, hFlush)
import Base

default (Int)

biFlush :: [SExpr] -> Lisp SExpr
biFlush [] = liftIO (hFlush stdout) >> return nil
biFlush _  = reportE' "no arguments required"

biGetLine :: [SExpr] -> Lisp SExpr
biGetLine [] = toString <$> liftIO getLine
biGetLine _  = reportE' "no arguments required"

biToString :: [SExpr] -> Lisp SExpr
biToString [arg] = return . toString $ show arg
biToString _     = reportE' "just one argument required"

biPutChar :: [SExpr] -> Lisp SExpr
biPutChar [SAtom _ (AChar c)]    = liftIO (putChar c) >> return nil
biPutChar [expr]                 = reportE (point expr) "char expected"
biPutChar _                      = reportE' "just one argument required"

builtinFunctions = [("flush",    Just 0, biFlush)
                   ,("get-line", Just 0, biGetLine)
                   ,("->string", Just 1, biToString)
                   ,("put-char", Just 1, biPutChar)]

specialOperators = []
