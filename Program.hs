module Program (Function(..), Program(..)) where

import qualified Data.Map as Map
import Data.Tree
import SemanticAnalyzer

data Function = Function [String] (Tree Terminal)

data Program = Program { functions :: Map.Map String Function,
                 body :: Tree Terminal }
