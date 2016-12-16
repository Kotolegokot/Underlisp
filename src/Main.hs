module Main where

import Data.Tree
import Control.Category
import System.Environment
import Interpreter


main :: IO ()
main = do
  args <- getArgs
  interprete_program (handle_args args)

handle_args :: [String] -> String
handle_args [filename] = filename
handle_args _          = error "usage: program filename"
