module Context (Context) where

import qualified Data.Map as Map
import SExpr

type Context = Map.Map String SExpr
