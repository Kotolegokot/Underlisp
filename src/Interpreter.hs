module Interpreter (interprete_program
                   , interprete_module
                   , interprete_module_no_prelude) where

import Data.Map (Map)
import qualified Reader
import qualified Evaluator
import SExpr
import Util

-- | a lisp interpretator is just a reader and evaluator joined together
interprete_program :: String -> IO ()
interprete_program filename = readFile filename >>= (Evaluator.evaluate_program . Reader.read (start_point filename))

interprete_module :: String -> IO (Map String SExpr)
interprete_module filename  = readFile filename >>= (Evaluator.evaluate_module . Reader.read (start_point filename))

interprete_module_no_prelude :: String -> IO (Map String SExpr)
interprete_module_no_prelude filename = readFile filename >>=
  (Evaluator.evaluate_module_no_prelude . Reader.read (start_point filename))

--interprete_repl :: IO ()
--interprete_repl = interprete_repl' (start_point "stdin")
--  where interprete_repl'
