module Lib.Environment (specialOperators) where

-- other
import qualified System.Posix.Env as E
import Data.IORef
import Control.Monad.IO.Class (liftIO)

-- local modules
import Base
import Evaluator
import Util

default (Int)

{-soEnv :: IORef Scope -> [SExpr] -> EvalM SExpr
soEnv scopeRef args = do
  symbols <- evalAloneSeq scopeRef args
  env <$> extractEnv scopeRef symbols-}

{-extractEnv :: IORef Scope -> [SExpr] -> EvalM (Map String Binding)
extractEnv scopeRef = foldM (\acc exp -> do key <- getSymbol exp
                                            result <- liftIO $ exploreIORefIO scopeRef $ scLookup key
                                            case result of
                                              Just value -> return $ Map.insert key value acc
                                              Nothing    -> reportE (point exp) $ "undefined symbol '" ++ key ++ "'")
                      Map.empty-}

{-soCurrentEnv :: IORef Scope -> [SExpr] -> EvalM SExpr
soCurrentEnv scopeRef [] = liftIO $ env <$> exploreIORef scopeRef getBindings
soCurrentEnv _        _  = reportE' "no arguments required"-}

{-biFunctionEnv :: IORef Scope -> [SExpr] -> EvalM SExpr
biFunctionEnv _ [SAtom _ (AProcedure (UserDefined localScope _ _ _))] = liftIO $ env <$> exploreIORef localScope getBindings
biFunctionEnv _ [sexpr]                                               = reportE (point sexpr) "a user-defined function expected"
biFunctionEnv _ _                                                     = reportE' "just one argument required"-}

soGetArgs :: IORef Scope -> [SExpr] -> EvalM SExpr
soGetArgs scopeRef [] = liftIO $ list . map string <$> exploreIORef scopeRef getCmdArgs
soGetArgs _        _  = reportE' "no arguments required"

soWithArgs :: IORef Scope -> [SExpr] -> EvalM SExpr
soWithArgs scopeRef (args:body) = do
  args' <- mapM getString =<< getList =<< evalAlone scopeRef args
  childScope <- liftIO $ newLocal scopeRef
  liftIO $ modifyIORef childScope (\scope -> scope { getCmdArgs = args' })
  evalBody childScope body
soWithArgs _        _           = reportE' "at least one argument required"

biGetEnv :: IORef Scope -> [SExpr] -> EvalM SExpr
biGetEnv _ [name] = do
  name' <- getString name
  result <- liftIO $ E.getEnv name'
  return $ case result of
    Just value -> SAtom (point name) (AString value)
    Nothing    -> nil
biGetEnv _ _ = reportE' "just one argument required"

biSetEnv :: IORef Scope -> [SExpr] -> EvalM SExpr
biSetEnv _ [name, value, rewrite] = do
  name' <- getString name
  value' <- getString value
  rewrite' <- getBool rewrite
  liftIO $ E.setEnv name' value' rewrite'
  return nil
biSetEnv _ _ = reportE' "three arguments required"

biUnsetEnv :: IORef Scope -> [SExpr] -> EvalM SExpr
biUnsetEnv _ [name] = do
  name' <- getString name
  liftIO $ E.unsetEnv name'
  return nil
biUnsetEnv _ _      = reportE' "just one argument required"

biGetEnvironment :: IORef Scope -> [SExpr] -> EvalM SExpr
biGetEnvironment _ [] = do
  environment <- liftIO E.getEnvironment
  return . list $ map (\(name, value) -> list [string name, string value]) environment
biGetEnvironment _ _  = reportE' "no arguments required"

biSetEnvironment :: IORef Scope -> [SExpr] -> EvalM SExpr
biSetEnvironment _ [l] = do
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
biSetEnvironment _ _   = reportE' "just one argument required"

specialOperators = [("get-args",        Just 0,  soGetArgs)
                   ,("with-args",       Nothing, soWithArgs)
                   ,("get-env",         Just 1,  withEvaluatedArgs biGetEnv)
                   ,("set-env",         Just 3,  withEvaluatedArgs biSetEnv)
                   ,("unset-env",       Just 1,  withEvaluatedArgs biUnsetEnv)
                   ,("get-environment", Just 0,  withEvaluatedArgs biGetEnvironment)
                   ,("set-environment", Just 1,  withEvaluatedArgs biSetEnvironment)]

