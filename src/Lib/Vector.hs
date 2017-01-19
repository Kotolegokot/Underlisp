module Lib.Vector (builtinFunctions
              ,specialOperators) where

import qualified Data.Vector as Vec
import Base

default (Int)

biVector :: [SExpr] -> Eval SExpr
biVector = return . vector . Vec.fromList

builtinFunctions = [("vector", Nothing,  biVector)]

specialOperators = []
