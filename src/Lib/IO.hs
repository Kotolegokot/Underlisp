module Lib.IO (builtinFunctions
              ,specialOperators) where

import Control.Monad.IO.Class (liftIO)

import System.IO (stdout, hFlush)
import Data.IORef
import Base

default (Int)

biFlush :: IORef Scope -> [SExpr] -> EvalM SExpr
biFlush _ [] = liftIO (hFlush stdout) >> return nil
biFlush _ _  = reportE' "no arguments required"

biGetLine :: IORef Scope -> [SExpr] -> EvalM SExpr
biGetLine _ [] = toString <$> liftIO getLine
biGetLine _ _  = reportE' "no arguments required"

biToString :: IORef Scope -> [SExpr] -> EvalM SExpr
biToString _ [arg] = return . toString $ show arg
biToString _ _     = reportE' "just one argument required"

biPutChar :: IORef Scope -> [SExpr] -> EvalM SExpr
biPutChar _ [SAtom _ (AChar c)] = liftIO (putChar c) >> return nil
biPutChar _ [expr]              = reportE (point expr) "char expected"
biPutChar _ _                   = reportE' "just one argument required"

builtinFunctions = [("flush",    Just 0, biFlush)
                   ,("get-line", Just 0, biGetLine)
                   ,("->string", Just 1, biToString)
                   ,("put-char", Just 1, biPutChar)]

specialOperators = []
