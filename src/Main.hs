module Main where

import System.Environment
import Interpreter
import Fail

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"]      -> do
      exec <- getExecutablePath
      putStrLn $ "usage: '" ++ exec ++ "' <filename>"
    (filename:args) -> interpreteProgram True filename args
    []              -> repl True
