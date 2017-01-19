module Lib.IO (builtinFunctions
              ,specialOperators) where

import Control.Monad.IO.Class (liftIO)

import System.IO (stdout, hFlush)
import Base

default (Int)

biFlush :: [SExpr] -> Eval SExpr
biFlush [] = liftIO (hFlush stdout) >> return nil
biFlush _  = reportUndef "no arguments required"

biGetLine :: [SExpr] -> Eval SExpr
biGetLine [] = toString <$> liftIO getLine
biGetLine _  = reportUndef "no arguments required"

biToString :: [SExpr] -> Eval SExpr
biToString [arg] = return . toString $ show arg
biToString _     = reportUndef "just one argument required"

biPutChar :: [SExpr] -> Eval SExpr
biPutChar [SAtom _ (AChar c)]    = liftIO (putChar c) >> return nil
biPutChar [expr]                 = report (point expr) "char expected"
biPutChar _                      = reportUndef "just one argument required"

builtinFunctions = [("flush",    Just 0, biFlush)
                   ,("get-line", Just 0, biGetLine)
                   ,("->string", Just 1, biToString)
                   ,("put-char", Just 1, biPutChar)]

specialOperators = []
