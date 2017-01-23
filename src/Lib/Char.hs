module Lib.Char (builtinFunctions
                ,specialOperators) where

import Prelude hiding (getChar)
import Data.IORef
import Data.Char (ord, chr)
import Base

default (Int)

biCharToInt :: IORef Scope -> [SExpr] -> Lisp SExpr
biCharToInt _ [exp] = int . ord <$> getChar exp
biCharToInt _ _     = reportE' "just one argument required"

biIntToChar :: IORef Scope -> [SExpr] -> Lisp SExpr
biIntToChar _ [exp] = char . chr <$> getInt exp
biIntToChar _ _     = reportE' "just one argument required"

builtinFunctions = [("char->int", Just 1, biCharToInt)
                   ,("int->char", Just 1, biIntToChar)]

specialOperators = []
