module Interpreter (interpreteProgram
                   ,interpreteModule
                   ,repl) where

import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, void)
import Control.Exception (throw)
import System.Console.Readline
import System.IO
import Data.Map (Map)
import qualified Reader as R
import qualified Evaluator as E
import Lib.Everything
import Base
import Point

preludePath :: String
preludePath = "stdlib/prelude.unlisp"

-- | A lisp interpretator is just a reader and evaluator joined together
interpreteProgram :: Bool -> String -> [String] -> IO ()
interpreteProgram prelude filename args = do
  e <- loadEnv prelude
  text <- readFile filename
  handleLisp e $ do
    read <- forwardExcept $ R.read (startPoint filename) text
    E.evaluateProgram args read

-- | Interprete a module and returns its lexical scope
interpreteModule :: Bool -> String -> Lisp (Map String EnvItem)
interpreteModule prelude filename = do
  e <- liftIO $ loadEnv prelude
  text <- liftIO $ readFile filename
  read <- forwardExcept $ R.read (startPoint filename) text
  E.evaluateModule read

-- | REPL (read-eval-print-loop) environment
repl :: Bool -> IO ()
repl prelude = void $ do
  e <- loadEnv prelude
  handleLines (startPoint "<repl>") e
    where handleLines :: Point -> Env -> IO ()
          handleLines p e = do
            line <- readline $ "[" ++ show (pRow p) ++ "]> "

            case line of
              Just line -> do
                unless (null line) $ addHistory line
                result <- evalLisp e $ do
                  read <- forwardExcept $ R.read p line
                  last <$> E.expandEvalSeq read

                exp <- case result of
                  Right val -> return val
                  Left  f   -> do
                    hPrint stderr f
                    return nil

                unless (isNil exp) (putStrLn $ "=> " ++ show exp)
                handleLines (forwardRow p) (linsert "it" (EnvSExpr exp) e)
              Nothing   -> putStrLn "Bye!"

-- | loads start environment
-- | no prelude if the first argument is false
loadEnv :: Bool -> IO Env
loadEnv True = do
  result <- evalLisp empty loadPrelude
  case result of
    Left fail -> throw fail
    Right val -> return val
loadEnv False = return startEnv

-- | loads prelude and start environment
loadPrelude :: Lisp Env
loadPrelude = do
  text <- liftIO $ (readFile preludePath)
  read <- forwardExcept $ R.read (startPoint preludePath) text
  E.expandEvalBody read
  get

-- | start environment
-- | contains built-in functions and special operators
startEnv :: Env
startEnv = envFromList $
  fmap (\(name, args, f) -> (name, EnvSExpr . procedure $ SpecialOp name args f []))
    (specialOperators ++
    [("env-from-file", Just 1, soEnvFromFile)
    ,("env-from-file-no-prelude", Just 1, soEnvFromFileNoPrelude)])
  ++
  fmap (\(name, args, f) -> (name, EnvSExpr . procedure $ BuiltIn name args f []))
    (builtinFunctions ++
    [("initial-env", Just 0, biInitialEnv)])

-- | loads environment from a file
soEnvFromFile :: [SExpr] -> Lisp SExpr
soEnvFromFile [sArg] = do
  str <- getString =<< E.eval sArg
  env <$> interpreteModule True str
soEnvFromFile _        = reportE' "just one argument required"

-- | loads environment from a file without prelude loaded
soEnvFromFileNoPrelude :: [SExpr] -> Lisp SExpr
soEnvFromFileNoPrelude [sArg] = do
  str <- getString =<< E.eval sArg
  env <$> interpreteModule False str
soEnvFromFileNoPrelude _       = reportE' "just one argument required"

-- | returns start environment plus prelude
biInitialEnv :: [SExpr] -> Lisp SExpr
biInitialEnv [] = do
  prelude <- loadPrelude
  return . env $ envMerge prelude
biInitialEnv _  = reportE' "no arguments requried"
