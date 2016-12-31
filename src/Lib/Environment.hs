{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Environment (spopEnv
                       , spopLoadEnv
                       , spopImportEnv
                       , spopCurrentEnv
                       , builtinFunctionEnv
                       , spopGetArgs
                       , spopWithArgs
                       , builtinGetEnv
                       , builtinSetEnv
                       , builtinUnsetEnv) where

import System.Posix.Env (setEnv, getEnv, unsetEnv)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Env
import qualified Reader
import Callable
import LexicalEnvironment
import SExpr
import LispShow
import Exception
import Util

spopEnv :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopEnv eval eval_scope e args = do
  pairs <- mapM (eval e) args
  let symbols = map snd pairs
  return $ if not $ all isSymbol symbols
           then reportUndef "symbol expected"
           else (e, env $ extractEnv e $ map (\s -> (fromSymbol s, point s)) symbols)

extractEnv :: LEnv SExpr -> [(String, Point)] -> Map String SExpr
extractEnv e keys = foldl (\acc (key, p) -> case Env.lookup key e of
                              Just value -> Map.insert key value acc
                              Nothing    -> report p $ "undefined symbol '" ++ key ++ "'")
                    Map.empty
                    keys

spopImportEnv :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopImportEnv eval _ e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom _ (AEnv add) -> (Env.xappend e add, nil)
    _                  -> report (point sexpr) "context expected"
spopImportEnv _   _ _ []     = reportUndef "just one argument required"

spopLoadEnv :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopLoadEnv eval _ e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom _ (AEnv add) -> (Env.lappend e add, nil)
    _                  -> report (point sexpr) "context expected"
spopLoadEnv _   _ _ []     = reportUndef "just one argument required"

spopCurrentEnv :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopCurrentEnv _ _ e [] = return (e, env $ Env.merge e)
spopCurrentEnv _ _ _ _  = reportUndef "no arguments required"

builtinFunctionEnv :: [SExpr] -> IO SExpr
builtinFunctionEnv [SAtom _ (ACallable (UserDefined e _ _ _))] = return . env $ Env.merge e
builtinFunctionEnv [sexpr]                                     = report (point sexpr) "function expected"
builtinFunctionEnv _                                           = reportUndef "just one argument required"

spopGetArgs :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopGetArgs _ _ e [] = return (e, list . map (list . map char) $ getArgs e)
spopGetArgs _ _ _ _  = reportUndef "no arguments required"

spopWithArgs :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopWithArgs eval evalScope e (args:sexprs) = do
  (_, args') <- eval e args
  let args'' = if isList args'
               then assureStrings $ fromList args'
               else report (point args) "list expected"
      e'     = setArgs args'' e
  (_, expr) <- evalScope e' sexprs
  return (e, expr)
spopWithArgs _    _         _ _             = reportUndef "at least one argument required"

builtinGetEnv :: [SExpr] -> IO SExpr
builtinGetEnv [name]
  | not $ isString name = report (point name) "string expected"
  | otherwise           = do
      result <- getEnv $ fromString name
      return $ case result of
        Just value -> toString value
        Nothing    -> nil
builtinGetEnv _ = reportUndef "just one argument required"

builtinSetEnv :: [SExpr] -> IO SExpr
builtinSetEnv [name, value, rewrite]
  | not $ isString name    = report (point name)    "string expected"
  | not $ isString value   = report (point value)   "string expected"
  | not $ isBool   rewrite = report (point rewrite) "bool expected"
  | otherwise              = setEnv (fromString name) (fromString value) (fromBool rewrite) >> return nil
builtinSetEnv _ = reportUndef "three arguments required"

builtinUnsetEnv :: [SExpr] -> IO SExpr
builtinUnsetEnv [name]
  | not $ isString name = report (point name) "string expected"
  | otherwise           = unsetEnv (fromString name) >> return nil
builtinUnsetEnv _ = reportUndef "just one argument required"
