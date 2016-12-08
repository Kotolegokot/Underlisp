module Context (Function(..), Context) where

import qualified Data.Map as Map
import Data.Tree
import SemanticAnalyzer

data Function = Function [String] (Tree Terminal)

type Context = Map.Map String Function
