{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter (interpreteProgram
                   , interpreteModule
                   , interpreteModuleNoPrelude
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
import Exception

-- | a lisp interpretator is just a reader and evaluator joined together
interpreteProgram :: String -> IO ()
interpreteProgram filename = readFile filename >>= (Evaluator.evaluateProgram . Reader.read (startPoint filename))

interpreteModule :: String -> IO (Map String SExpr)
interpreteModule filename  = readFile filename >>= (Evaluator.evaluateModule . Reader.read (startPoint filename))

interpreteModuleNoPrelude :: String -> IO (Map String SExpr)
interpreteModuleNoPrelude filename = readFile filename >>=
  (Evaluator.evaluateModuleNoPrelude . Reader.read (startPoint filename))

repl :: IO ()
repl = do
  prelude <- Evaluator.loadPrelude
  handleLines (startPoint "<interactive>") prelude
  where handleLines :: Point -> LEnv SExpr -> IO ()
        handleLines p e = do
          putStr $ "[" ++ show (pRow p) ++ "]> "
          hFlush stdout
          handle (\(err :: IOError) -> if isEOFError err
                                           then putStrLn "\nBye"
                                           else ioError err) $ do
            line <- getLine
            (e', expr) <- catch (Evaluator.evalScope e $ Reader.read p line)
                          (\err -> do hPutStrLn stderr $ show (err :: LispError)
                                      return (e, nil))
            putStrLn $ "=> " ++ lispShow expr
            handleLines (forwardRow p) e'
