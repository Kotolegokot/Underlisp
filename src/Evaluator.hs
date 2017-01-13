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
--preludePath = "examples/test2.unlisp"

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

-- | expands macros and evaluates a scope
expandAndEvalScope :: Env -> [SExpr] -> IO (Env, SExpr)
expandAndEvalScope e sexprs = do
  let (e', sexprs') = collectMacros (pass e) sexprs
  sexprs'' <- expandMacros e' sexprs'
  foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e', nil) sexprs''

-- | evaluates a lexical scope
evalScope :: Env -> [SExpr] -> IO (Env, SExpr)
evalScope e = foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e, nil)

-- | takes a scope and evaluates all top-level
-- | defmacros in it
collectMacros :: Env -> [SExpr] -> (Env, [SExpr])
collectMacros e xs = foldl (\(accE, accSexprs) sexpr -> case parseDefmacro accE sexpr of
                               Just (name, macro) -> (linsert name (EnvMacro macro) accE, accSexprs)
                               Nothing            -> (accE, accSexprs ++ [sexpr]))
                     (e, [])
                     xs

-- | expands all top-level macros
expandMacros :: Env -> [SExpr] -> IO [SExpr]
expandMacros e (x:xs) = do
  expr <- expandMacro e x
  rest <- expandMacros e xs
  return (expr : rest)
expandMacros _ []     = return []

data EMState = Default | Backquote

-- | expands one macro expression recursively

expandMacro :: Env -> SExpr -> IO SExpr
expandMacro = expandMacro' Default

expandMacro' :: EMState -> Env -> SExpr -> IO SExpr
expandMacro' Default e l@(SList p (first@(SAtom _ (ASymbol sym)):rest))
  | sym == "quote"       = return l
  | sym == "backquote"   = do
      rest' <- mapM (expandMacro' Backquote e) rest
      return $ SList p (first:rest')
  | sym == "interpolate" = reportCmd p "interpolate" "calling out of backquote"
  | otherwise = case lookupMacro (fromSymbol first) e of
      Just m@(Macro _ _ _ _ _) -> do
        expr <- callMacro p expandAndEvalScope e m rest
        expandMacro' Default e expr
      Nothing                  -> do
        list' <- mapM (expandMacro' Default e) (first:rest)
        return $ SList p list'
expandMacro' Default e l@(SList p (first:rest)) = do
  first' <- expandMacro' Default e first
  if isSymbol first'
    then expandMacro' Default e $ SList p (first':rest)
    else do
      rest' <- mapM (expandMacro' Default e) rest
      return $ SList p (first':rest')
expandMacro' Default _ other                    = return other
expandMacro' Backquote e l@(SList p (first@(SAtom _ (ASymbol sym)):rest))
  | sym == "interpolate" = do
      rest' <- mapM (expandMacro' Default e) rest
      return $ SList p (first:rest')
  | otherwise            = return l
expandMacro' Backquote _ other = return other

-- | parses a defmacro expression
parseDefmacro :: Env -> SExpr -> Maybe (String, Macro)
parseDefmacro e (SList p (SAtom defmacroPoint (ASymbol "defmacro"):name:lambdaList:body))
  | not $ isSymbol name = reportCmd (point name) "defmacro" "string expected"
  | otherwise           = return (fromSymbol name, Macro p e prototype body [])
  where prototype = parseLambdaList lambdaList

parseDefmacro _ (SList p (SAtom _ (ASymbol "defmacro"):_)) = reportCmd p "defmacro" "at least two arguments required"
parseDefmacro _ _                                          = Nothing

-- | evaluates a lexical scope as if it is the same
-- | lexical level
evalScopeInterpolated :: Env -> [SExpr] -> IO (Env, SExpr)
evalScopeInterpolated e sexprs = do
  let (e', sexprs') = collectMacros e sexprs
  sexprs'' <- expandMacros e' sexprs'
  foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e', nil) sexprs''

-- | evaluates an s-expression
eval :: Env -> SExpr -> IO (Env, SExpr)
eval e (SList _ (first:args))  = do
  (_, first') <- eval e first
  rethrow (\le -> if lePoint le == Undefined
                  then le { lePoint = point first }
                  else le) $
    if isProcedure first'
    then eval' $ fromProcedure first'
    else report (point first) $ "unable to execute s-expression: '" ++ show first' ++ "'"
  where eval' c | isUserDefined c || isBuiltIn c = do
                    pairs <- mapM (eval e) args
                    call (point first) eval evalScope e c (map snd pairs)
                | isSpecialOp c     = call (point first) eval evalScope e c args
eval e (SAtom p (ASymbol "_")) = report p "addressing '_' is forbidden"
eval e (SAtom p (ASymbol sym)) = case envLookup sym e of
  Just (EnvSExpr s) -> return (e, setPoint s p)
  _                 -> report p $ "undefined identificator '" ++ sym ++ "'"
eval e sexpr                   = return (e, sexpr)

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
