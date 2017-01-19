module Lib.Char (builtinFunctions
                ,specialOperators) where

import Prelude hiding (getChar)
import Data.Char (ord, chr)
import Base

default (Int)

biCharToInt :: [SExpr] -> Eval SExpr
biCharToInt [exp] = int . ord <$> getChar exp
biCharToInt _     = reportUndef "just one argument required"

biIntToChar :: [SExpr] -> Eval SExpr
biIntToChar [exp] = char . chr <$> getInt exp
biIntToChar _     = reportUndef "just one argument required"

builtinFunctions = [("char->int", Just 1, biCharToInt)
                   ,("int->char", Just 1, biIntToChar)]

specialOperators = []
