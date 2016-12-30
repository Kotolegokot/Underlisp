module Main where

import System.Environment
import Interpreter
import Exception

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"]      -> do
      exec <- getExecutablePath
      putStrLn $ "usage: '" ++ exec ++ "' <filename>"
    (filename:args) -> interpreteProgram filename args
    []              -> repl
