{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Evaluator where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Env
import LexicalEnvironment
import qualified Reader
import Control.Arrow
import Control.Monad (void, foldM)
import Control.Monad (when)
import SExpr
import Util
import Callable
import LispShow
import Lib.Everything
import Point
import Exception

preludePath = "stdlib/prelude.lisp" :: String

-- | evaluates a module and returns nothing
evaluateProgram :: [SExpr] -> IO ()
evaluateProgram body = do
  prelude <- loadPrelude
  void $ evalScope prelude body

-- | evaluates a module
evaluateModule :: [SExpr] -> IO (Map String SExpr)
evaluateModule body = do
  prelude <- loadPrelude
  (e, _) <- evalScope prelude body
  return $ Env.merge e

-- | evaluates a module without prelude loaded
evaluateModuleNoPrelude :: [SExpr] -> IO (Map String SExpr)
evaluateModuleNoPrelude body = do
  (e, _) <- evalScope startEnv body
  return $ Env.merge e

-- | evaluates a lexical scope
evalScope :: LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
evalScope e = foldM (\(prevE, _) sexpr -> eval prevE sexpr) (Env.pass e, nil)

-- | evaluates an s-expression
eval :: LEnv SExpr -> SExpr -> IO (LEnv SExpr, SExpr)
eval e (SList p (first:rest))  = do
  (_, first') <- eval e first
  rethrow (\le -> if lePoint le == Undefined
                  then le { lePoint = p }
                  else le) $ case first' of
    SAtom _ (ACallable (UserDefined localE prototype sexprs bound))      -> do
      pairs <- mapM (eval e) rest
      let argBindings = bindArgs prototype (bound ++ map snd pairs)
      (_, expr) <- evalScope (Env.lappend localE argBindings) sexprs
      return (e, replacePoint expr p)
    SAtom _ (ACallable (Macro localE prototype sexprs bound))            -> do
      let argBindings = bindArgs prototype (bound ++ rest)
      (_, expr) <- evalScope (Env.lappend localE argBindings) sexprs
      (e', expr') <- eval e expr
      return (e', replacePoint expr' p)
    SAtom _ (ACallable (BuiltIn name _ f bound))                          ->
      rethrow (\le -> if null $ leCmd le
                      then le { leCmd = name }
                      else le) $ do
        pairs <- mapM (eval e) rest
        result <- f (bound ++ map snd pairs)
        return (e, replacePoint result p)
    SAtom _ (ACallable (SpecialOp name _ f bound))                        ->
      rethrow (\le -> if null $ leCmd le
                      then le { leCmd = name }
                      else le) $ do
        (e', expr) <- f eval evalScope e (bound ++ rest)
        return (e', replacePoint expr p)
    _-> report p $ "unable to execute s-expression: '" ++ lispShow first' ++ "'"
eval e (SAtom p (ASymbol "_")) = report p "addressing '_' is forbidden"
eval e (SAtom p (ASymbol sym)) = case Env.lookup sym e of
  Just value -> return (e, replacePoint value p)
  Nothing    -> report p $ "undefined identificator '" ++ sym ++ "'"
eval e sexpr                   = return (e, sexpr)

-- | loads prelude and start environment
loadPrelude :: IO (LEnv SExpr)
loadPrelude = do
  text <- readFile preludePath
  (e, _) <- evalScope startEnv $ Reader.read (startPoint preludePath) text
  return e

-- | start environment
-- | contains built-in functions and special operators
startEnv :: LEnv SExpr
startEnv = Env.fromList $
    (fmap (\(name, args, f) -> (name, callable $ SpecialOp name args f [])) [
    ("gensym",                       Just 0,  spopGensym),
    ("let",                          Nothing, spopLet),
    ("if",                           Just 3,  spopIf),
    ("set",                          Just 2,  spopSet),
    ("lambda",                       Nothing, spopLambda),
    ("macro",                        Nothing, spopMacro),
    ("macro-expand",                 Just 1,  spopMacroExpand),
    ("bind",                         Nothing, spopBind),
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
    ("seq",                          Nothing, spopSeq) ]) ++
  (fmap (\(name, args, f) -> (name, callable $ BuiltIn name args f [])) [
    ("type",             Just 1,  builtinType),
    ("put-char",         Just 1,  builtinPutChar),
    ("write",            Just 1,  builtinWrite),
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
    ("function-env",     Just 1,  builtinFunctionEnv) ])

-- | loads environment from a file
spopEnvFromFile :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
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
spopEnvFromFileNoPrelude :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
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
  return . env $ Env.merge prelude
builtinInitialEnv _  = reportUndef "no arguments requried"
