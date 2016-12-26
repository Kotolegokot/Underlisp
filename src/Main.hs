module Main where

import Data.Tree
import Control.Category
import System.Environment
import Interpreter

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> interprete_program filename
    []         -> repl
    _          -> error "usage: program filename"
