{-# LANGUAGE ScopedTypeVariables #-}
module Interpreter (interpreteProgram
                   ,interpreteModule
                   ,repl) where

import Control.Exception
import Control.Conditional (cond)
import Control.Monad (unless)
import System.IO
import System.IO.Error (isEOFError)
import Data.Map (Map)
import qualified Reader as R
import qualified Evaluator as E
import Lib.Everything
import Base
import Point
import Exception

preludePath :: String
preludePath = "stdlib/prelude.unlisp"

-- | a lisp interpretator is just a reader and evaluator joined together
interpreteProgram :: Bool -> String -> [String] -> IO ()
interpreteProgram prelude filename args = do
  e <- loadEnv prelude
  text <- readFile filename
  E.evaluateProgram e args $ R.read (startPoint filename) text

-- | interpretes a module and returns its lexical scope
interpreteModule :: Bool -> String -> IO (Map String EnvItem)
interpreteModule prelude filename = do
  e <- loadEnv prelude
  text <- readFile filename
  E.evaluateModule e $ R.read (startPoint filename) text

-- | REPL (read-eval-print-loop) environment
repl :: Bool -> IO ()
repl prelude = do
  e <- loadEnv prelude
  handleLines (startPoint "<repl>") e
  where handleLines :: Point -> Env -> IO ()
        handleLines p e = do
          putStr $ "[" ++ show (pRow p) ++ "]> "
          hFlush stdout
          handle (\(err :: IOError) -> if isEOFError err
                                       then putStrLn "\nBye"
                                       else ioError err) $ do
            line <- getLine
            (e', expr) <- catch (E.expandAndEvalScopeInterpolated e $ R.read p line)
                          (\err -> do hPutStrLn stderr $ show (err :: LispError)
                                      return (e, nil))
            unless (isNil expr) $
              putStrLn $ "=> " ++ show expr
            handleLines (forwardRow p) (linsert "it" (EnvSExpr expr) e')

-- | loads start environment
-- | no prelude if the first argument is false
loadEnv :: Bool -> IO Env
loadEnv True  = loadPrelude
loadEnv False = return startEnv

-- | loads prelude and start environment
loadPrelude :: IO Env
loadPrelude = do
  text <- readFile preludePath
  (e, _) <- E.expandAndEvalScope startEnv $ R.read (startPoint preludePath) text
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
soEnvFromFile :: Env -> [SExpr] -> IO (Env, SExpr)
soEnvFromFile e [sArg] = do
  (_, arg) <- E.eval e sArg
  if not $ isString arg
    then report (point sArg) "string expected"
    else do
      e' <- interpreteModule True $ fromString arg
      return (e, env e')
soEnvFromFile _ _        = reportUndef "just one argument required"

-- | loads environment from a file without prelude loaded
soEnvFromFileNoPrelude :: Env -> [SExpr] -> IO (Env, SExpr)
soEnvFromFileNoPrelude e [sArg] = do
  (_, arg) <- E.eval e sArg
  if not $ isString arg
    then report (point sArg) "string expected"
    else do
      e' <- interpreteModule False $ fromString arg
      return (e, env e')
soEnvFromFileNoPrelude _ _       = reportUndef "just one argument required"

-- | returns start environment plus prelude
biInitialEnv :: [SExpr] -> IO SExpr
biInitialEnv [] = do
  prelude <- loadPrelude
  return . env $ envMerge prelude
biInitialEnv _  = reportUndef "no arguments requried"
