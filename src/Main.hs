module Main where

import System.IO
import System.Environment
import Interpreter
import Exception

main :: IO ()
main = do
  args <- getArgs
  handle_lisp_error $ case args of
    [filename] -> interprete_program filename
    []         -> repl
    _          -> do
      exec <- getExecutablePath
      hPutStrLn stderr $ "usage: '" ++ exec ++ "' <filename>"
