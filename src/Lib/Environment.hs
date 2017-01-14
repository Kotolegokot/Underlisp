module Lib.Environment (builtinFunctions
                       ,specialOperators) where

import System.Posix.Env
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Reader
import Base
import Exception
import Point
import Evaluator

soEnv :: Env -> [SExpr] -> IO (Env, SExpr)
soEnv e args = do
  pairs <- mapM (eval e) args
  let symbols = map snd pairs
  return (e, env $ extractEnv e symbols)

extractEnv :: Env -> [SExpr] -> Map String EnvItem
extractEnv e keys = foldl (\acc sexpr -> if not $ isSymbol sexpr
                                         then report (point sexpr) "symbol expected"
                                         else let key = fromSymbol sexpr
                                              in  case envLookup key e of
                                                    Just value -> Map.insert key value acc
                                                    Nothing    -> report (point sexpr) $ "undefined symbol '" ++ key ++ "'")
                    Map.empty
                    keys

soImportEnv :: Env -> [SExpr] -> IO (Env, SExpr)
soImportEnv e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom _ (AEnv add) -> (xappend e add, nil)
    _                  -> report (point sexpr) "context expected"
soImportEnv _ []     = reportUndef "just one argument required"

soLoadEnv :: Env -> [SExpr] -> IO (Env, SExpr)
soLoadEnv e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom _ (AEnv add) -> (lappend e add, nil)
    _                  -> report (point arg) "context expected"
soLoadEnv _ []    = reportUndef "just one argument required"

soCurrentEnv :: Env -> [SExpr] -> IO (Env, SExpr)
soCurrentEnv e [] = return (e, env $ envMerge e)
soCurrentEnv _ _  = reportUndef "no arguments required"

biFunctionEnv :: [SExpr] -> IO SExpr
biFunctionEnv [SAtom _ (AProcedure (UserDefined e _ _ _))] = return . env $ envMerge e
biFunctionEnv [sexpr]                                     = report (point sexpr) "function expected"
biFunctionEnv _                                           = reportUndef "just one argument required"

soGetArgs :: Env -> [SExpr] -> IO (Env, SExpr)
soGetArgs e [] = return (e, list . map (list . map char) $ getArgs e)
soGetArgs _ _  = reportUndef "no arguments required"

soWithArgs :: Env -> [SExpr] -> IO (Env, SExpr)
soWithArgs e (args:sexprs) = do
  (_, args') <- eval e args
  let args'' = if isList args'
               then assureStrings $ fromList args'
               else report (point args) "list expected"
      e'     = setArgs e args''
  (_, expr) <- evalScope e' sexprs
  return (e, expr)
soWithArgs _ _             = reportUndef "at least one argument required"

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

assureStrings :: [SExpr] -> [String]
assureStrings = foldl (\acc s -> if isString s
                                 then acc ++ [fromString s]
                                 else report (point s) "string expected")
                []
