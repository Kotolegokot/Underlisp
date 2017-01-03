module Lib.Environment (builtinFunctions
                       ,specialOperators) where

import System.Posix.Env
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Reader
import Base
import Exception
import Point
import Util

soEnv :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soEnv eval eval_scope e args = do
  pairs <- mapM (eval e) args
  let symbols = map snd pairs
  return $ if not $ all isSymbol symbols
           then reportUndef "symbol expected"
           else (e, env $ extractEnv e $ map (\s -> (fromSymbol s, point s)) symbols)

extractEnv :: Env -> [(String, Point)] -> Map String SExpr
extractEnv e keys = foldl (\acc (key, p) -> case envLookup key e of
                              Just value -> Map.insert key value acc
                              Nothing    -> report p $ "undefined symbol '" ++ key ++ "'")
                    Map.empty
                    keys

soImportEnv :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soImportEnv eval _ e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom _ (AEnv add) -> (xappend e add, nil)
    _                  -> report (point sexpr) "context expected"
soImportEnv _   _ _ []     = reportUndef "just one argument required"

soLoadEnv :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soLoadEnv eval _ e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom _ (AEnv add) -> (lappend e add, nil)
    _                  -> report (point sexpr) "context expected"
soLoadEnv _   _ _ []     = reportUndef "just one argument required"

soCurrentEnv :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soCurrentEnv _ _ e [] = return (e, env $ envMerge e)
soCurrentEnv _ _ _ _  = reportUndef "no arguments required"

biFunctionEnv :: [SExpr] -> IO SExpr
biFunctionEnv [SAtom _ (ACallable (UserDefined e _ _ _))] = return . env $ envMerge e
biFunctionEnv [sexpr]                                     = report (point sexpr) "function expected"
biFunctionEnv _                                           = reportUndef "just one argument required"

soGetArgs :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soGetArgs _ _ e [] = return (e, list . map (list . map char) $ getArgs e)
soGetArgs _ _ _ _  = reportUndef "no arguments required"

soWithArgs :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soWithArgs eval evalScope e (args:sexprs) = do
  (_, args') <- eval e args
  let args'' = if isList args'
               then assureStrings $ fromList args'
               else report (point args) "list expected"
      e'     = setArgs e args''
  (_, expr) <- evalScope e' sexprs
  return (e, expr)
soWithArgs _    _         _ _             = reportUndef "at least one argument required"

biGetEnv :: [SExpr] -> IO SExpr
biGetEnv [name]
  | not $ isString name = report (point name) "string expected"
  | otherwise           = do
      result <- getEnv $ fromString name
      return $ case result of
        Just value -> toString value
        Nothing    -> nil
biGetEnv _ = reportUndef "just one argument required"

biSetEnv :: [SExpr] -> IO SExpr
biSetEnv [name, value, rewrite]
  | not $ isString name    = report (point name)    "string expected"
  | not $ isString value   = report (point value)   "string expected"
  | not $ isBool   rewrite = report (point rewrite) "bool expected"
  | otherwise              = setEnv (fromString name) (fromString value) (fromBool rewrite) >> return nil
biSetEnv _ = reportUndef "three arguments required"

biUnsetEnv :: [SExpr] -> IO SExpr
biUnsetEnv [name]
  | not $ isString name = report (point name) "string expected"
  | otherwise           = unsetEnv (fromString name) >> return nil
biUnsetEnv _ = reportUndef "just one argument required"

biGetEnvironment :: [SExpr] -> IO SExpr
biGetEnvironment [] = do
  environment <- getEnvironment
  return . list $ map (\(name, value) -> list [toString name, toString value]) environment
biGetEnvironment _  = reportUndef "no arguments required"

biSetEnvironment :: [SExpr] -> IO SExpr
biSetEnvironment [l] = do
  let pList = if isList l
              then assurePairList $ fromList l
              else report (point l) "list expected"
  setEnvironment pList
  return nil
  where assurePairList []            = []
        assurePairList (SList _ [str1, str2]:xs)
          | not $ isString str1 = report (point str1) "string expected"
          | not $ isString str2 = report (point str2) "string expected"
          | otherwise           = (fromString str1, fromString str2) : assurePairList xs
        assurePairList (SList p _:_) = report p "pair expected"
        assurePairList (x:_)         = report (point x) "list expected"
biSetEnvironment _      = reportUndef "just one argument required"

builtinFunctions = [("function-env",    Just (1 :: Int), biFunctionEnv)
                   ,("get-env",         Just 1,          biGetEnv)
                   ,("set-env",         Just 3,          biSetEnv)
                   ,("unset-env",       Just 1,          biUnsetEnv)
                   ,("get-environment", Just 0,          biGetEnvironment)
                   ,("set-environment", Just 1,          biSetEnvironment)]

specialOperators = [("env",          Nothing,          soEnv)
                   ,("load-env",     Just (1 :: Int),  soLoadEnv)
                   ,("import-env",   Just 1,           soImportEnv)
                   ,("current-env",  Just 0,           soCurrentEnv)
                   ,("get-args",     Just 0,           soGetArgs)
                   ,("with-args",    Nothing,          soWithArgs)]
