module Lib.Environment (builtinFunctions
                       ,specialOperators) where

import qualified System.Posix.Env as E
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State
import Control.Monad.IO.Class (liftIO)
import Control.Monad (foldM)
import Base
import Evaluator

default (Int)

soEnv :: [SExpr] -> Lisp SExpr
soEnv args = do
  symbols <- mapM evalAlone args
  env <$> extractEnv symbols

extractEnv :: [SExpr] -> Lisp (Map String EnvItem)
extractEnv = foldM (\acc exp -> do key <- getSymbol exp
                                   result <- gets $ envLookup key
                                   case result of
                                     Just value -> return $ Map.insert key value acc
                                     Nothing    -> reportE (point exp) $ "undefined symbol '" ++ key ++ "'")
             Map.empty

soImportEnv :: [SExpr] -> Lisp SExpr
soImportEnv [arg] = do
  add <- getEnv =<< evalAlone arg
  modify $ xappend add
  return nil
soImportEnv _     = reportE' "just one argument required"

soLoadEnv :: [SExpr] -> Lisp SExpr
soLoadEnv [arg] = do
  add <- getEnv =<< evalAlone arg
  modify $ lappend add
  return nil
soLoadEnv _     = reportE' "just one argument required"

soCurrentEnv :: [SExpr] -> Lisp SExpr
soCurrentEnv [] = env . envMerge <$> get
soCurrentEnv _  = reportE' "no arguments required"

biFunctionEnv :: [SExpr] -> Lisp SExpr
biFunctionEnv [SAtom _ (AProcedure (UserDefined e _ _ _))] = return . env $ envMerge e
biFunctionEnv [sexpr]                                      = reportE (point sexpr) "function expected"
biFunctionEnv _                                            = reportE' "just one argument required"

soGetArgs :: [SExpr] -> Lisp SExpr
soGetArgs [] = list . map toString <$> gets getArgs
soGetArgs _  = reportE' "no arguments required"

soWithArgs :: [SExpr] -> Lisp SExpr
soWithArgs (args:body) = do
  args' <- evalAlone args
  args'' <- mapM getString =<< getList args'
  previousArgs <- gets getArgs
  modify $ setArgs args''
  result <- evalBody body
  modify $ setArgs previousArgs
  return result
soWithArgs _           = reportE' "at least one argument required"

biGetEnv :: [SExpr] -> Lisp SExpr
biGetEnv [name] = do
  name' <- getString name
  result <- liftIO $ E.getEnv name'
  return $ case result of
    Just value -> toString value
    Nothing    -> nil
biGetEnv _ = reportE' "just one argument required"

biSetEnv :: [SExpr] -> Lisp SExpr
biSetEnv [name, value, rewrite] = do
  name' <- getString name
  value' <- getString value
  rewrite' <- getBool rewrite
  liftIO $ E.setEnv name' value' rewrite'
  return nil
biSetEnv _ = reportE' "three arguments required"

biUnsetEnv :: [SExpr] -> Lisp SExpr
biUnsetEnv [name] = do
  name' <- getString name
  liftIO $ E.unsetEnv name'
  return nil
biUnsetEnv _ = reportE' "just one argument required"

biGetEnvironment :: [SExpr] -> Lisp SExpr
biGetEnvironment [] = do
  environment <- liftIO E.getEnvironment
  return . list $ map (\(name, value) -> list [toString name, toString value]) environment
biGetEnvironment _  = reportE' "no arguments required"

biSetEnvironment :: [SExpr] -> Lisp SExpr
biSetEnvironment [l] = do
  pList <- assurePairList =<< getList l
  liftIO $ E.setEnvironment pList
  return nil
  where assurePairList []            = return []
        assurePairList (SList _ [str1, str2]:xs) = do
          xs' <- assurePairList xs
          str1' <- getString str1
          str2' <- getString str2
          return ((str1', str2') : xs')
        assurePairList (SList p _:_) = reportE p "pair expected"
        assurePairList (x:_)         = reportE (point x) "list expected"
biSetEnvironment _      = reportE' "just one argument required"

builtinFunctions = [("function-env",    Just 1, biFunctionEnv)
                   ,("get-env",         Just 1, biGetEnv)
                   ,("set-env",         Just 3, biSetEnv)
                   ,("unset-env",       Just 1, biUnsetEnv)
                   ,("get-environment", Just 0, biGetEnvironment)
                   ,("set-environment", Just 1, biSetEnvironment)]

specialOperators = [("env",          Nothing, soEnv)
                   ,("load-env",     Just 1,  soLoadEnv)
                   ,("import-env",   Just 1,  soImportEnv)
                   ,("current-env",  Just 0,  soCurrentEnv)
                   ,("get-args",     Just 0,  soGetArgs)
                   ,("with-args",    Nothing, soWithArgs)]
