module Lib.List (specialOperators) where

import Data.IORef

-- local modules
import Base
import Evaluator

default (Int)

biList :: IORef Scope -> [SExpr] -> EvalM SExpr
biList _ = return . list

specialOperators = [("list",   Nothing, withEvaluatedArgs biList)]
