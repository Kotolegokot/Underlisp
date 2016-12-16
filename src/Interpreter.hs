module Interpreter (interprete_program, interprete_module) where

import qualified Reader
import qualified Evaluator
import SExpr
import Util

-- | a lisp interpretator is just a reader and evaluator joined together
interprete_program :: String -> IO ()
interprete_program filename = readFile filename >>= (Evaluator.evaluate_program . Reader.read (start_point filename))

interprete_module :: String -> IO Context
interprete_module filename  = readFile filename >>= (Evaluator.evaluate_module . Reader.read (start_point filename))

--interprete_stdin :: IO ()
--interprete_stdin = interprete_stdin' (start_point "stdin")
--  where interprete_stdin'
