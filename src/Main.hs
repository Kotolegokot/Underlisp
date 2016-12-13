module Main where

import Data.Tree
import System.Environment
import Interpreter

main :: IO ()
main = getArgs >>= handle_args >>= interprete_program

handle_args :: [String] -> IO String
handle_args []     = getLine
handle_args [file] = readFile file
handle_args _      = getExecutablePath >>= (\exe -> error $ "usage: " ++ exe ++ " [file]")

