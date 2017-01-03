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

preludePath = "stdlib/prelude.lisp" :: String

-- | evaluates a module and returns nothing
evaluateProgram :: [SExpr] -> [String] -> IO ()
evaluateProgram body args = do
  prelude <- loadPrelude
  void $ evalScope (setArgs prelude args) body

-- | evaluates a module
evaluateModule :: [SExpr] -> IO (Map String SExpr)
evaluateModule body = do
  prelude <- loadPrelude
  (e, _) <- evalScope prelude body
  return $ lexical e

-- | evaluates a module without prelude loaded
evaluateModuleNoPrelude :: [SExpr] -> IO (Map String SExpr)
evaluateModuleNoPrelude body = do
  (e, _) <- evalScope startEnv body
  return $ lexical e

-- | evaluates a lexical scope
evalScope :: Env -> [SExpr] -> IO (Env, SExpr)
evalScope e = foldM (\(prevE, _) sexpr -> eval prevE sexpr) (pass e, nil)

-- | evaluates an s-expression
eval :: Env -> SExpr -> IO (Env, SExpr)
eval e (SList _ (first:args))  = do
  (_, first') <- eval e first
  rethrow (\le -> if lePoint le == Undefined
                  then le { lePoint = point first }
                  else le) $
    if isCallable first'
    then eval' $ fromCallable first'
    else report (point first) $ "unable to execute s-expression: '" ++ show first' ++ "'"
  where eval' c | isUserDefined c || isBuiltIn c = do
                    pairs <- mapM (eval e) args
                    call (point first) eval evalScope e c (map snd pairs)
                | isMacro c || isSpecialOp c     = call (point first) eval evalScope e c args
eval e (SAtom p (ASymbol "_")) = report p "addressing '_' is forbidden"
eval e (SAtom p (ASymbol sym)) = case envLookup sym e of
  Just value -> return (e, setPoint value p)
  Nothing    -> report p $ "undefined identificator '" ++ sym ++ "'"
eval e sexpr                   = return (e, sexpr)

-- | loads prelude and start environment
loadPrelude :: IO Env
loadPrelude = do
  text <- readFile preludePath
  (e, _) <- evalScope startEnv $ Reader.read (startPoint preludePath) text
  return e

-- | start environment
-- | contains built-in functions and special operators
startEnv :: Env
startEnv = envFromList $
  (fmap (\(name, args, f) -> (name, callable $ SpecialOp name args f [])) $
    specialOperators ++
    [("env-from-file", Just 1, soEnvFromFile)
    ,("env-from-file-no-prelude", Just 1, soEnvFromFileNoPrelude)]) ++
  (fmap (\(name, args, f) -> (name, callable $ BuiltIn name args f [])) $
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
