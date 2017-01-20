module Lib.Char (builtinFunctions
                ,specialOperators) where

import Prelude hiding (getChar)
import Data.Char (ord, chr)
import Base

default (Int)

biCharToInt :: [SExpr] -> Lisp SExpr
biCharToInt [exp] = int . ord <$> getChar exp
biCharToInt _     = reportE' "just one argument required"

biIntToChar :: [SExpr] -> Lisp SExpr
biIntToChar [exp] = char . chr <$> getInt exp
biIntToChar _     = reportE' "just one argument required"

builtinFunctions = [("char->int", Just 1, biCharToInt)
                   ,("int->char", Just 1, biIntToChar)]

specialOperators = []
