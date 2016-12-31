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
  return $ envMerge e

-- | evaluates a module without prelude loaded
evaluateModuleNoPrelude :: [SExpr] -> IO (Map String SExpr)
evaluateModuleNoPrelude body = do
  (e, _) <- evalScope startEnv body
  return $ envMerge e

-- | evaluates a lexical scope
evalScope :: Env -> [SExpr] -> IO (Env, SExpr)
evalScope e = foldM (\(prevE, _) sexpr -> eval prevE sexpr) (pass e, nil)

-- | evaluates an s-expression
eval :: Env -> SExpr -> IO (Env, SExpr)
eval e (SList _ (first:args))  = do
  (_, first') <- eval e first
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
    (fmap (\(name, args, f) -> (name, callable $ SpecialOp name args f [])) [
    ("gensym",                       Just 0,  spopGensym),
    ("let",                          Nothing, spopLet),
    ("if",                           Just 3,  spopIf),
    ("set",                          Just 2,  spopSet),
    ("lambda",                       Nothing, spopLambda),
    ("macro",                        Nothing, spopMacro),
    ("macro-expand",                 Just 1,  spopMacroExpand),
    ("bind",                         Nothing, spopBind),
    ("apply",                        Just 2,  spopApply),
    ("quote",                        Just 1,  spopQuote),
    ("backquote",                    Just 1,  spopBackquote),
    ("interprete",                   Just 1,  spopInterprete),
    ("eval",                         Just 1,  spopEval),
    ("and",                          Nothing, spopAnd),
    ("or",                           Nothing, spopOr),
    ("->",                           Just 2,  spopImpl),
    ("env",                          Nothing, spopEnv),
    ("load-env",                     Just 1,  spopLoadEnv),
    ("import-env",                   Just 1,  spopImportEnv),
    ("current-env",                  Just 0,  spopCurrentEnv),
    ("env-from-file",                Just 1,  spopEnvFromFile),
    ("env-from-file-no-prelude",     Just 1,  spopEnvFromFileNoPrelude),
    ("defined?",                     Just 1,  spopDefined),
    ("get-args",                     Just 0,  spopGetArgs),
    ("with-args",                    Nothing, spopWithArgs),
    ("seq",                          Nothing, spopSeq) ]) ++
  (fmap (\(name, args, f) -> (name, callable $ BuiltIn name args f [])) [
    ("type",             Just 1,  builtinType),
    ("put-char",         Just 1,  builtinPutChar),
    ("to-string",        Just 1,  builtinToString),
    ("flush",            Just 0,  builtinFlush),
    ("get-line",         Just 0,  builtinGetLine),
    ("list",             Nothing, builtinList),
    ("head",             Just 1,  builtinHead),
    ("tail",             Just 1,  builtinTail),
    ("null",             Just 1,  builtinNull),
    ("append",           Just 2,  builtinAppend),
    ("+",                Nothing, builtinSum),
    ("-",                Just 2,  builtinSubstract),
    ("*",                Nothing, builtinProduct),
    ("/",                Just 2,  builtinDivide),
    ("float",            Just 1,  builtinFloat),
    ("exp",              Just 1,  builtinExp),
    ("ln",               Just 1,  builtinLn),
    ("^",                Just 2,  builtinPower),
    ("sin",              Just 1,  builtinSin),
    ("cos",              Just 1,  builtinCos),
    ("asin",             Just 1,  builtinASin),
    ("acos",             Just 1,  builtinACot),
    ("atan",             Just 1,  builtinATan),
    ("acot",             Just 1,  builtinACot),
    ("sinh",             Just 1,  builtinSinH),
    ("cosh",             Just 1,  builtinCosH),
    ("asinh",            Just 1,  builtinASinH),
    ("acosh",            Just 1,  builtinACosH),
    ("atanh",            Just 1,  builtinATanH),
    ("acoth",            Just 1,  builtinACotH),
    ("truncate",         Just 1,  builtinTruncate),
    ("round",            Just 1,  builtinRound),
    ("ceiling",          Just 1,  builtinCeiling),
    ("floor",            Just 1,  builtinFloor),
    ("not-a-number?",    Just 1,  builtinIsNan),
    ("infinite?",        Just 1,  builtinIsInfinite),
    ("denormalized?",    Just 1,  builtinIsDenormalized),
    ("negative-zero?",   Just 1,  builtinIsNegativeZero),
    ("IEEE?",            Just 1,  builtinIsIEEE),
    ("quot-rem",         Just 2,  builtinQuotRem),
    ("div-mod",          Just 2,  builtinDivMod),
    ("not",              Just 1,  builtinNot),
    ("=",                Just 2,  builtinEQ),
    ("<",                Just 2,  builtinLT),
    ("error",            Just 1,  builtinError),
    ("initial-env",      Just 0,  builtinInitialEnv),
    ("function-env",     Just 1,  builtinFunctionEnv),
    ("get-env",          Just 1,  builtinGetEnv),
    ("set-env",          Just 3,  builtinSetEnv),
    ("unset-env",        Just 1,  builtinUnsetEnv),
    ("get-environment",  Just 0,  builtinGetEnvironment),
    ("set-environment",  Just 1,  builtinSetEnvironment) ])

-- | loads environment from a file
spopEnvFromFile :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopEnvFromFile eval eval_scope e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SList p l -> do
      let filename = map fromChar l
      text <- readFile filename
      e' <- evaluateModule $ Reader.read p text
      return (e, env e')
    other     -> report (point other) "string expected"
spopEnvFromFile _    _          _        _    = reportUndef "just one argument required"

-- | loads environment from a file without prelude loaded
spopEnvFromFileNoPrelude :: Eval -> EvalScope  -> Env -> [SExpr] -> IO (Env, SExpr)
spopEnvFromFileNoPrelude eval evalScope e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SList p list -> do
      let filename = map fromChar list
      text <- readFile filename
      e' <- evaluateModuleNoPrelude $ Reader.read p text
      return (e, env e')
    other        -> report (point other) "string expected"
spopEnvFromFileNoPrelude _    _          _       _      = reportUndef "just one argument required"

-- | returns start environment plus prelude
builtinInitialEnv :: [SExpr] -> IO SExpr
builtinInitialEnv [] = do
  prelude <- loadPrelude
  return . env $ envMerge prelude
builtinInitialEnv _  = reportUndef "no arguments requried"
