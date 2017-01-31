module Lib.Char (specialOperators) where

import Prelude hiding (getChar)
import Data.IORef
import Data.Char (ord, chr)

-- local modules
import Evaluator
import Base

default (Int)

biCharToInt :: IORef Scope -> [SExpr] -> EvalM SExpr
biCharToInt _ [exp] = int . ord <$> getChar exp
biCharToInt _ _     = reportE' "just one argument required"

biIntToChar :: IORef Scope -> [SExpr] -> EvalM SExpr
biIntToChar _ [exp] = char . chr <$> getInt exp
biIntToChar _ _     = reportE' "just one argument required"

specialOperators = [("char->int", Just 1, withEvaluatedArgs biCharToInt)
                   ,("int->char", Just 1, withEvaluatedArgs biIntToChar)]
