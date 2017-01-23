module Interpreter (interpreteProgram
                   ,interpreteModule
                   ,repl) where

-- map
import qualified Data.Map as Map
import Data.Map (Map)

-- other
import Data.IORef
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import System.Console.Readline
import System.IO

-- locale modules
import qualified Reader as R
import qualified Evaluator as E
import Lib.Everything
import Base
import Point
import Util

preludePath :: String
preludePath = "stdlib/prelude.unlisp"

-- | A lisp interpretator is just a reader and evaluator joined together
interpreteProgram :: Bool -> String -> [String] -> IO ()
interpreteProgram prelude filename args = do
  scope <- loadEnv prelude
  modifyIORef scope (setCmdArgs args)
  text <- readFile filename
  handleLisp $ do
    exps <- forwardExcept $ R.read (startPoint filename) text
    E.expandEvalBody scope exps

-- | Interprete a module and returns its lexical scope
interpreteModule :: Bool -> String -> Lisp (Map String Binding)
interpreteModule prelude filename = do
  scope <- liftIO $ loadEnv prelude
  text <- liftIO $ readFile filename
  exps <- forwardExcept $ R.read (startPoint filename) text
  E.expandEvalBody scope exps
  liftIO $ exploreIORef scope getBindings

-- | REPL (read-eval-print-loop) environment
repl :: Bool -> IO ()
repl prelude = handleLines (startPoint "<REPL>") =<< loadEnv prelude
    where handleLines :: Point -> IORef Scope -> IO ()
          handleLines p scopeRef = do
            line <- readline $ "[" ++ show (pRow p) ++ "]> "

            case line of
              Just line -> do
                unless (null line) $ addHistory line
                result <- runLisp $ do
                  exps <- forwardExcept $ R.read p line
                  E.expandEvalBody scopeRef exps

                exp <- case result of
                  Right val -> return val
                  Left  f   -> do
                    hPrint stderr f
                    return nil

                unless (isNil exp) (putStrLn $ "=> " ++ show exp)
                modifyIORef scopeRef (scInsert "it" $ BSExpr exp)
                handleLines (forwardRow p) scopeRef
              Nothing   -> putStrLn "Bye!"

-- | Load start environment.
-- No prelude if the first argument is false
loadEnv :: Bool -> IO (IORef Scope)
loadEnv True  = loadPrelude
loadEnv False = newIORef $ newGlobal' startEnv []

-- | loads prelude and start environment
loadPrelude :: IO (IORef Scope)
loadPrelude = do
  text <- readFile preludePath
  global <- newIORef $ newGlobal' startEnv []
  let result = runExcept $ R.read (startPoint preludePath) text
  case result of
    Right exps -> handleLisp $ E.expandEvalBody global exps
    Left fail  -> hPrint stderr fail
  return global

-- | start environment
-- | contains built-in functions and special operators
startEnv :: Map String Binding
startEnv = Map.fromList $
  fmap (\(name, args, f) -> (name, BSExpr . procedure $ SpecialOp name args f []))
    (specialOperators ++
    [("env-from-file", Just 1, soEnvFromFile)
    ,("env-from-file-no-prelude", Just 1, soEnvFromFileNoPrelude)])
  ++
  fmap (\(name, args, f) -> (name, BSExpr . procedure $ BuiltIn name args f []))
    (builtinFunctions ++
    [("initial-env", Just 0, biInitialEnv)])

-- | loads environment from a file
soEnvFromFile :: IORef Scope -> [SExpr] -> Lisp SExpr
soEnvFromFile scopeRef [sArg] = do
  str <- getString =<< E.eval scopeRef sArg
  env <$> interpreteModule True str
soEnvFromFile _        _      = reportE' "just one argument required"

-- | loads environment from a file without prelude loaded
soEnvFromFileNoPrelude :: IORef Scope -> [SExpr] -> Lisp SExpr
soEnvFromFileNoPrelude scopeRef [sArg] = do
  str <- getString =<< E.eval scopeRef sArg
  env <$> interpreteModule False str
soEnvFromFileNoPrelude _        _      = reportE' "just one argument required"

-- | returns start environment plus prelude
biInitialEnv :: IORef Scope -> [SExpr] -> Lisp SExpr
biInitialEnv _ [] = liftIO $ do
  scope <- loadPrelude
  env <$> exploreIORef scope getBindings
biInitialEnv _ _  = reportE' "no arguments requried"
