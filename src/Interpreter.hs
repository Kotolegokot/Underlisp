{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter (interprete_program
                   , interprete_module
                   , interprete_module_no_prelude
                   , repl) where

import Control.Exception
import System.IO
import System.IO.Error (isEOFError)
import Data.Map (Map)
import qualified Reader
import qualified Evaluator
import LexicalEnvironment
import SExpr
import Util
import Point
import LispShow

-- | a lisp interpretator is just a reader and evaluator joined together
interprete_program :: String -> IO ()
interprete_program filename = readFile filename >>= (Evaluator.evaluate_program . Reader.read (start_point filename))

interprete_module :: String -> IO (Map String SExpr)
interprete_module filename  = readFile filename >>= (Evaluator.evaluate_module . Reader.read (start_point filename))

interprete_module_no_prelude :: String -> IO (Map String SExpr)
interprete_module_no_prelude filename = readFile filename >>=
  (Evaluator.evaluate_module_no_prelude . Reader.read (start_point filename))

repl :: IO ()
repl = do
  prelude <- Evaluator.load_prelude
  handle_lines (start_point "<interactive>") prelude
  where handle_lines :: Point -> LEnv SExpr -> IO ()
        handle_lines p e = do
          putStr $ "[" ++ show (p_row p) ++ "]> "
          hFlush stdout
          handle (\(err :: IOError) -> if isEOFError err
                                           then putStrLn "\nBye"
                                           else ioError err) $ do
            line <- getLine
            (e', expr) <- catch (Evaluator.eval_scope e $ Reader.read p line)
                          (\err -> do hPutStrLn stderr $ show (err :: ErrorCall)
                                      return (e, nil))
            putStrLn $ "=> " ++ lisp_show expr
            handle_lines (forward_row p) e'
