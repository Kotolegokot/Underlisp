{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter (interpreteProgram
                   , interpreteModule
                   , interpreteModuleNoPrelude
                   , repl) where

import Control.Exception
import Control.Conditional (cond)
import System.IO
import System.IO.Error (isEOFError)
import Data.Map (Map)
import qualified Reader
import qualified Evaluator
import Base
import Util
import Point
import Exception

-- | a lisp interpretator is just a reader and evaluator joined together
interpreteProgram :: String -> [String] -> IO ()
interpreteProgram filename args = do
  text <- readFile filename
  let sexprs = Reader.read (startPoint filename) text
  Evaluator.evaluateProgram sexprs args

interpreteModule :: String -> IO (Map String EnvItem)
interpreteModule filename = readFile filename >>= (Evaluator.evaluateModule . Reader.read (startPoint filename))

interpreteModuleNoPrelude :: String -> IO (Map String EnvItem)
interpreteModuleNoPrelude filename = readFile filename >>=
  (Evaluator.evaluateModuleNoPrelude . Reader.read (startPoint filename))

repl :: IO ()
repl = do
  prelude <- Evaluator.loadPrelude
  handleLines (startPoint "<interactive>") prelude
  where handleLines :: Point -> Env -> IO ()
        handleLines p e = do
          putStr $ "[" ++ show (pRow p) ++ "]> "
          hFlush stdout
          handle (\(err :: IOError) -> if isEOFError err
                                           then putStrLn "\nBye"
                                           else ioError err) $ do
            line <- getLine
            (e', expr) <- catch (Evaluator.evalScopeInterpolated e $ Reader.read p line)
                          (\err -> do hPutStrLn stderr $ show (err :: LispError)
                                      return (e, nil))
            putStrLn $ "=> " ++ show expr
            handleLines (forwardRow p) (linsert "it" (EnvSExpr expr) e')
