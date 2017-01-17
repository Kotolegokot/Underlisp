module Interpreter (interpreteProgram
                   ,interpreteModule
                   ,repl) where

import Control.Exception (handle)
import Control.Conditional (cond)
import Control.Monad (unless, void)
import System.Console.Readline
import System.IO
import System.IO.Error (isEOFError)
import Data.Map (Map)
import qualified Reader as R
import qualified Evaluator as E
import Lib.Everything
import Base
import Point
import Fail

preludePath :: String
preludePath = "stdlib/prelude.unlisp"

-- | a lisp interpretator is just a reader and evaluator joined together
interpreteProgram :: Bool -> String -> [String] -> IO ()
interpreteProgram prelude filename args = handleEval $do
  e <- loadEnv prelude
  text <- liftIO $ readFile filename
  read <- R.read (startPoint filename) text
  E.evaluateProgram e args read

-- | interpretes a module and returns its lexical scope
interpreteModule :: Bool -> String -> Eval (Map String EnvItem)
interpreteModule prelude filename = do
  e <- loadEnv prelude
  text <- liftIO $ readFile filename
  read <- R.read (startPoint filename) text
  E.evaluateModule e read

-- | REPL (read-eval-print-loop) environment
repl :: Bool -> IO ()
repl prelude = void $ runEval $ do
  e <- loadEnv prelude
  liftIO $ handleLines (startPoint "<repl>") e
    where handleLines :: Point -> Env -> IO ()
          handleLines p e = do
            line <- readline $ "[" ++ show (pRow p) ++ "]> "

            case line of
              Nothing   -> putStrLn "\nBye!" >> return ()
              Just line -> do
                unless (null line) $ addHistory line
                (result, callstack) <- runEval $ do
                  read <- R.read p line
                  E.expandAndEvalScopeInterpolated e read

                (e', expr) <- case result of
                  Right val -> return (val :: (Env, SExpr))
                  Left  f   -> do
                    printStack callstack
                    hPrint stderr f
                    return (e, nil)

                unless (isNil expr) (putStrLn $ "=> " ++ show expr)
                handleLines (forwardRow p) (linsert "it" (EnvSExpr expr) e')

-- | loads start environment
-- | no prelude if the first argument is false
loadEnv :: Bool -> Eval Env 
loadEnv True  = loadPrelude
loadEnv False = return startEnv

-- | loads prelude and start environment
loadPrelude :: Eval Env
loadPrelude = do
  text <- liftIO (readFile preludePath)
  read <- R.read (startPoint preludePath) text
  (e, _) <- E.expandAndEvalScope startEnv read
  return e

-- | start environment
-- | contains built-in functions and special operators
startEnv :: Env
startEnv = envFromList $
  (fmap (\(name, args, f) -> (name, EnvSExpr . procedure $ SpecialOp name args f [])) $
    specialOperators ++
    [("env-from-file", Just 1, soEnvFromFile)
    ,("env-from-file-no-prelude", Just 1, soEnvFromFileNoPrelude)]) ++
  (fmap (\(name, args, f) -> (name, EnvSExpr . procedure $ BuiltIn name args f [])) $
    builtinFunctions ++
    [("initial-env", Just 0, biInitialEnv)])

-- | loads environment from a file
soEnvFromFile :: Env -> [SExpr] -> Eval (Env, SExpr)
soEnvFromFile e [sArg] = do
  (_, arg) <- E.eval e sArg
  if not $ isString arg
    then report (point sArg) "string expected"
    else do
      e' <- interpreteModule True $ fromString arg
      return (e, env e')
soEnvFromFile _ _        = reportUndef "just one argument required"

-- | loads environment from a file without prelude loaded
soEnvFromFileNoPrelude :: Env -> [SExpr] -> Eval (Env, SExpr)
soEnvFromFileNoPrelude e [sArg] = do
  (_, arg) <- E.eval e sArg
  if not $ isString arg
    then report (point sArg) "string expected"
    else do
      e' <- interpreteModule False $ fromString arg
      return (e, env e')
soEnvFromFileNoPrelude _ _       = reportUndef "just one argument required"

-- | returns start environment plus prelude
biInitialEnv :: [SExpr] -> Eval SExpr
biInitialEnv [] = do
  prelude <- loadPrelude
  return . env $ envMerge prelude
biInitialEnv _  = reportUndef "no arguments requried"
