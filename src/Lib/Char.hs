module Lib.Char (builtinCharToInt
                ,builtinIntToChar) where

import Data.Char (ord, chr)
import Base
import Exception

builtinCharToInt :: [SExpr] -> IO SExpr
builtinCharToInt [c]
  | not $ isChar c = report (point c) "char expected"
  | otherwise      = return . int . ord $ fromChar c
builtinCharToInt _ = reportUndef "just one argument required"

builtinIntToChar :: [SExpr] -> IO SExpr
builtinIntToChar [i]
  | not $ isInt i = report (point i) "int expected"
  | otherwise     = return . char . chr $ fromInt i
builtinIntToChar _ = reportUndef "just one argument required"
