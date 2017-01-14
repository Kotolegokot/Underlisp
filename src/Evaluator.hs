module Evaluator where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Reader
import Control.Arrow
import Control.Monad (void, foldM, when)
import Base
import Lib.Everything
import Point
import Exception
import Util

preludePath = "stdlib/prelude.unlisp" :: String

-- | evaluates a module and returns nothing
evaluateProgram :: [SExpr] -> [String] -> IO ()
evaluateProgram body args = do
  prelude <- loadPrelude
  void $ expandAndEvalScope (setArgs prelude args) body

-- | evaluates a module
evaluateModule :: [SExpr] -> IO (Map String EnvItem)
evaluateModule body = do
  prelude <- loadPrelude
  (e, _) <- expandAndEvalScope prelude body
  return $ lexical e

-- | evaluates a module without prelude loaded
evaluateModuleNoPrelude :: [SExpr] -> IO (Map String EnvItem)
evaluateModuleNoPrelude body = do
  (e, _) <- expandAndEvalScope startEnv body
  return $ lexical e

-- | evaluates a lexical scope as if it is the same
-- | lexical level
evalScopeInterpolated :: Env -> [SExpr] -> IO (Env, SExpr)
evalScopeInterpolated e sexprs = do
  let (e', sexprs') = collectMacros e sexprs
  sexprs'' <- expandMacros e' sexprs'
  foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e', nil) sexprs''

-- | loads prelude and start environment
loadPrelude :: IO Env
loadPrelude = do
  text <- readFile preludePath
  (e, _) <- expandAndEvalScope startEnv $ Reader.read (startPoint preludePath) text
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
soEnvFromFile :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soEnvFromFile eval eval_scope e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SList p l -> do
      let filename = map fromChar l
      text <- readFile filename
      e' <- evaluateModule $ Reader.read p text
      return (e, env e')
    other     -> report (point other) "string expected"
soEnvFromFile _    _          _        _    = reportUndef "just one argument required"

-- | loads environment from a file without prelude loaded
soEnvFromFileNoPrelude :: Eval -> EvalScope  -> Env -> [SExpr] -> IO (Env, SExpr)
soEnvFromFileNoPrelude eval evalScope e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SList p list -> do
      let filename = map fromChar list
      text <- readFile filename
      e' <- evaluateModuleNoPrelude $ Reader.read p text
      return (e, env e')
    other        -> report (point other) "string expected"
soEnvFromFileNoPrelude _    _          _       _      = reportUndef "just one argument required"

-- | returns start environment plus prelude
biInitialEnv :: [SExpr] -> IO SExpr
biInitialEnv [] = do
  prelude <- loadPrelude
  return . env $ envMerge prelude
biInitialEnv _  = reportUndef "no arguments requried"
