module Lib.Vector (builtinFunctions
              ,specialOperators) where

-- vector
import qualified Data.Vector as Vec
--import Data.Vector (Vector)

-- other
import Data.IORef

-- local modules
import Base

default (Int)

biVector :: IORef Scope -> [SExpr] -> EvalM SExpr
biVector _ = return . vector . Vec.fromList

builtinFunctions = [("vector", Nothing,  biVector)]

specialOperators = []
