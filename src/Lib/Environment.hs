module Lib.Environment (builtinFunctions
                       ,specialOperators) where

import qualified System.Posix.Env as E
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (foldM)
import Base
import Evaluator

default (Int)

soEnv :: Env -> [SExpr] -> Lisp (Env, SExpr)
soEnv e args = do
  pairs <- mapM (eval e) args
  let symbols = map snd pairs
  extracted <- extractEnv e symbols
  return (e, env extracted)

extractEnv :: Env -> [SExpr] -> Lisp (Map String EnvItem)
extractEnv e = foldM (\acc exp -> do key <- getSymbol exp
                                     case envLookup key e of
                                       Just value -> return $ Map.insert key value acc
                                       Nothing    -> reportE (point exp) $ "undefined symbol '" ++ key ++ "'")
               Map.empty

soImportEnv :: Env -> [SExpr] -> Lisp (Env, SExpr)
soImportEnv e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SAtom _ (AEnv add) -> return (xappend e add, nil)
    _                  -> reportE (point sexpr) "context expected"
soImportEnv _ _     = reportE' "just one argument required"

soLoadEnv :: Env -> [SExpr] -> Lisp (Env, SExpr)
soLoadEnv e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SAtom _ (AEnv add) -> return (lappend e add, nil)
    _                  -> reportE (point arg) "context expected"
soLoadEnv _ _     = reportE' "just one argument required"

soCurrentEnv :: Env -> [SExpr] -> Lisp (Env, SExpr)
soCurrentEnv e [] = return (e, env $ envMerge e)
soCurrentEnv _ _  = reportE' "no arguments required"

biFunctionEnv :: [SExpr] -> Lisp SExpr
biFunctionEnv [SAtom _ (AProcedure (UserDefined e _ _ _))] = return . env $ envMerge e
biFunctionEnv [sexpr]                                     = reportE (point sexpr) "function expected"
biFunctionEnv _                                           = reportE' "just one argument required"

soGetArgs :: Env -> [SExpr] -> Lisp (Env, SExpr)
soGetArgs e [] = return (e, list . map toString $ getArgs e)
soGetArgs _ _  = reportE' "no arguments required"

soWithArgs :: Env -> [SExpr] -> Lisp (Env, SExpr)
soWithArgs e (args:sexprs) = do
  (_, args') <- eval e args
  args'' <- mapM getString =<< getList args' 
  let e' = setArgs e args''
  (_, expr) <- evalScope e' sexprs
  return (e, expr)
soWithArgs _ _             = reportE' "at least one argument required"

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
