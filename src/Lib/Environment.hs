module Lib.Environment (builtinFunctions
                       ,specialOperators) where

import System.Posix.Env
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (foldM)
import qualified Reader
import Base
import Point
import Evaluator

soEnv :: Env -> [SExpr] -> Eval (Env, SExpr)
soEnv e args = do
  pairs <- mapM (eval e) args
  let symbols = map snd pairs
  extracted <- extractEnv e symbols
  return (e, env extracted)

extractEnv :: Env -> [SExpr] -> Eval (Map String EnvItem)
extractEnv e keys = foldM (\acc sexpr -> if not $ isSymbol sexpr
                                         then report (point sexpr) "symbol expected"
                                         else let key = fromSymbol sexpr
                                              in  case envLookup key e of
                                                    Just value -> return $ Map.insert key value acc
                                                    Nothing    -> report (point sexpr) $ "undefined symbol '" ++ key ++ "'")
                    Map.empty
                    keys

soImportEnv :: Env -> [SExpr] -> Eval (Env, SExpr)
soImportEnv e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SAtom _ (AEnv add) -> return (xappend e add, nil)
    _                  -> report (point sexpr) "context expected"
soImportEnv _ []     = reportUndef "just one argument required"

soLoadEnv :: Env -> [SExpr] -> Eval (Env, SExpr)
soLoadEnv e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SAtom _ (AEnv add) -> return (lappend e add, nil)
    _                  -> report (point arg) "context expected"
soLoadEnv _ []    = reportUndef "just one argument required"

soCurrentEnv :: Env -> [SExpr] -> Eval (Env, SExpr)
soCurrentEnv e [] = return (e, env $ envMerge e)
soCurrentEnv _ _  = reportUndef "no arguments required"

biFunctionEnv :: [SExpr] -> Eval SExpr
biFunctionEnv [SAtom _ (AProcedure (UserDefined e _ _ _))] = return . env $ envMerge e
biFunctionEnv [sexpr]                                     = report (point sexpr) "function expected"
biFunctionEnv _                                           = reportUndef "just one argument required"

soGetArgs :: Env -> [SExpr] -> Eval (Env, SExpr)
soGetArgs e [] = return (e, list . map (list . map char) $ getArgs e)
soGetArgs _ _  = reportUndef "no arguments required"

soWithArgs :: Env -> [SExpr] -> Eval (Env, SExpr)
soWithArgs e (args:sexprs) = do
  (_, args') <- eval e args
  args'' <- if isList args'
            then assureStrings $ fromList args'
            else report (point args) "list expected"
  let e' = setArgs e args''
  (_, expr) <- evalScope e' sexprs
  return (e, expr)
soWithArgs _ _             = reportUndef "at least one argument required"

biGetEnv :: [SExpr] -> Eval SExpr
biGetEnv [name]
  | not $ isString name = report (point name) "string expected"
  | otherwise           = do
      result <- liftIO $ getEnv (fromString name)
      return $ case result of
        Just value -> toString value
        Nothing    -> nil
biGetEnv _ = reportUndef "just one argument required"

biSetEnv :: [SExpr] -> Eval SExpr
biSetEnv [name, value, rewrite]
  | not $ isString name    = report (point name)    "string expected"
  | not $ isString value   = report (point value)   "string expected"
  | not $ isBool   rewrite = report (point rewrite) "bool expected"
  | otherwise              = do
      liftIO $ setEnv (fromString name) (fromString value) (fromBool rewrite)
      return nil
biSetEnv _ = reportUndef "three arguments required"

biUnsetEnv :: [SExpr] -> Eval SExpr
biUnsetEnv [name]
  | not $ isString name = report (point name) "string expected"
  | otherwise           = liftIO (unsetEnv (fromString name)) >> return nil
biUnsetEnv _ = reportUndef "just one argument required"

biGetEnvironment :: [SExpr] -> Eval SExpr
biGetEnvironment [] = do
  environment <- liftIO getEnvironment
  return . list $ map (\(name, value) -> list [toString name, toString value]) environment
biGetEnvironment _  = reportUndef "no arguments required"

biSetEnvironment :: [SExpr] -> Eval SExpr
biSetEnvironment [l] = do
  pList <- if isList l
           then assurePairList $ fromList l
           else report (point l) "list expected"
  liftIO $ setEnvironment pList
  return nil
  where assurePairList []            = return []
        assurePairList (SList _ [str1, str2]:xs)
          | not $ isString str1 = report (point str1) "string expected"
          | not $ isString str2 = report (point str2) "string expected"
          | otherwise           = do
              xs' <- assurePairList xs
              return ((fromString str1, fromString str2) : xs')
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

assureStrings :: [SExpr] -> Eval [String]
assureStrings = foldM (\acc s -> if isString s
                                 then return $ acc ++ [fromString s]
                                 else report (point s) "string expected")
                []
