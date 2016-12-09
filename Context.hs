module Context (Context, empty) where

import qualified Data.Map as Map
import SExpr

type Context = Map.Map String SExpr

empty :: Context
empty = Map.empty
