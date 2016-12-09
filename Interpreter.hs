module Interpreter (interprete) where

import Text.Read
import Control.Monad
import Data.Tree
import System.IO
import qualified Data.Map as Map

import SExpr
import qualified Reader
import qualified Evaluator

-- | a lisp interpretator is just a reader and evaluator joined together
interprete :: String -> IO ()
interprete = Evaluator.evaluate . Reader.read
