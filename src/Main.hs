module Main where

import System.IO
import System.Environment
import Interpreter
import Exception

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> interpreteProgram filename
    []         -> repl
    _          -> do
      exec <- getExecutablePath
      hPutStrLn stderr $ "usage: '" ++ exec ++ "' <filename>"
