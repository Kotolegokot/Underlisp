module Lib.Char (builtinFunctions
                ,specialOperators) where

import Data.Char (ord, chr)
import Base
import Exception

biCharToInt :: [SExpr] -> IO SExpr
biCharToInt [c]
  | not $ isChar c = report (point c) "char expected"
  | otherwise      = return . int . ord $ fromChar c
biCharToInt _ = reportUndef "just one argument required"

biIntToChar :: [SExpr] -> IO SExpr
biIntToChar [i]
  | not $ isInt i = report (point i) "int expected"
  | otherwise     = return . char . chr $ fromInt i
biIntToChar _ = reportUndef "just one argument required"

builtinFunctions = [("char->int", Just (1 :: Int), biCharToInt)
                   ,("int->char", Just 1,          biIntToChar)]

specialOperators = []
