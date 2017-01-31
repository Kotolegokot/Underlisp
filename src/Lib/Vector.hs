module Lib.Vector (specialOperators) where

-- vector
import qualified Data.Vector as Vec
--import Data.Vector (Vector)

-- other
import Data.IORef

-- local modules
import Base
import Evaluator

default (Int)

biVector :: IORef Scope -> [SExpr] -> EvalM SExpr
biVector _ = return . vector . Vec.fromList

specialOperators = [("vector", Nothing,  withEvaluatedArgs biVector)]
