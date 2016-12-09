module Context (Context) where

import qualified Data.Map as Map
import Data.Tree
import SemanticAnalyzer

type Context = Map.Map String Atom
