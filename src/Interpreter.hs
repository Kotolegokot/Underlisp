module Interpreter (interprete_program, interprete_module) where

import qualified Reader
import qualified Evaluator
import SExpr

-- | a lisp interpretator is just a reader and evaluator joined together
interprete_program :: String -> IO ()
interprete_program = Evaluator.evaluate_program . Reader.read

interprete_module :: String -> IO Context
interprete_module = Evaluator.evaluate_module . Reader.read
