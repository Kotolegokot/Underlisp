module Main where

import System.IO
import System.Environment
import Interpreter

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> interprete_program filename
    []         -> repl
    _          -> do
      exec <- getExecutablePath
      hPutStrLn stderr $ "usage: '" ++ exec ++ "' <filename>"
