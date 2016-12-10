module Interpreter (interprete) where

import qualified Reader
import qualified Evaluator

-- | a lisp interpretator is just a reader and evaluator joined together
interprete :: String -> IO ()
interprete = Evaluator.evaluate . Reader.read
