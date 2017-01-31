module Lib.Main (specialOperators) where

-- map
import qualified Data.Map as Map
--import Data.Map (Map)

-- other
import Control.Monad.IO.Class (liftIO)
import Data.IORef

-- local modules
import Base
import Evaluator
import Type
import Util

default (Int)

-- | Special operator lambda
-- (lambda lambda-list [body])
soLambda :: IORef Scope -> [SExpr] -> EvalM SExpr
soLambda scopeRef (lambdaList:body) = do
  prototype <- parseLambdaList lambdaList
  return . procedure $ UserDefined scopeRef prototype body []
soLambda _        []                = reportE' "at least one argument expected"

-- | Special operator let
soLet :: IORef Scope -> [SExpr] -> EvalM SExpr
soLet scopeRef (SList p pairs : body) = do
  childScope <- liftIO $ newLocal scopeRef
  putBindings childScope pairs
  last <$> evalSeq childScope body
    where putBindings :: IORef Scope -> [SExpr] -> EvalM ()
          putBindings scopeRef exps = do
            add <- Map.fromList <$> mapM (putBinding scopeRef) exps
            liftIO $ modifyIORef scopeRef (scAppend add)

          putBinding :: IORef Scope -> SExpr -> EvalM (String, SExpr)
          putBinding scopeRef (SList _ [SAtom _ (ASymbol var), lambdaList, body]) = do
            value <- soLambda scopeRef [lambdaList, body]
            return (var, value)
          putBinding scopeRef (SList _ [SAtom _ (ASymbol var), value]) = do
            exp <- evalAlone scopeRef value
            return (var, exp)
          putBinding _        (SList _ (exp1:_))  = reportE (point exp1) "first item in a binding pair must be a keyword"
          putBinding _        other               = reportE (point other) "(var value) pair expected"
soLet _        [expr]                    = reportE (point expr) "list expected"
soLet _        _                         = reportE' "at least one argument expected"

soIsDef :: IORef Scope -> [SExpr] -> EvalM SExpr
soIsDef scopeRef [sKey] = do
  key <- getSymbol =<< evalAlone scopeRef sKey
  liftIO $ bool <$> exploreIORefIO scopeRef (scMember key)
soIsDef _        _      = reportE' "just one argument required"

soUndef :: IORef Scope -> [SExpr] -> EvalM SExpr
soUndef scopeRef [sKey] = do
  key <- getSymbol =<< evalAlone scopeRef sKey
  liftIO $ modifyIORefIO scopeRef (scDelete key)
  return nil
soUndef _        _      = reportE' "just one argument required"

-- | Special operator define
soDefine :: IORef Scope -> [SExpr] -> EvalM SExpr
soDefine scopeRef [sKey] = do
  key <- getSymbol =<< evalAlone scopeRef sKey
  liftIO $ modifyIORef scopeRef (scInsert key nil)
  return nil
soDefine scopeRef [sKey, sValue] = do
  key <- getSymbol =<< evalAlone scopeRef sKey
  value <- evalAlone scopeRef sValue
  liftIO $ modifyIORef scopeRef (scInsert key value)
  return nil
soDefine _        _              = reportE' "two arguments required"

-- | Special operator set
soSet :: IORef Scope -> [SExpr] -> EvalM SExpr
soSet scopeRef [sKey, sValue] = do
  key <- getSymbol =<< evalAlone scopeRef sKey
  result <- liftIO $ exploreIORefIO scopeRef (scLookup key)
  case result of
    Just (SAtom _ (AProcedure BuiltIn {})) -> reportE' "rebinding special operators is forbidden"
    Just _                                   -> do
      value <- evalAlone scopeRef sValue
      liftIO $ modifyIORefIO scopeRef (scSet key value)
      return nil
    Nothing                                  -> reportE' $ "undefined identificator '" ++ key ++ "'"
soSet _        _              = reportE' "two arguments required"

-- | Built-in function type
biType :: IORef Scope -> [SExpr] -> EvalM SExpr
biType _ [exp] = return . symbol . showType $ exp
biType _ _     = reportE' "just one argument required"

soBind :: IORef Scope -> [SExpr] -> EvalM SExpr
soBind scopeRef (first:args) = do
  pr <- getProcedure =<< evalAlone scopeRef first
  case pr of
    so@BuiltIn {} -> procedure <$> bind so args
    other           -> do
      args' <- evalAloneSeq scopeRef args
      procedure <$> bind other args'
soBind _        []            = reportE' "at least one argument required"

-- | Special operator apply
soApply :: IORef Scope -> [SExpr] -> EvalM SExpr
soApply scopeRef (first:args@(_:_)) = do
  pr <- getProcedure =<< evalAlone scopeRef first
  args' <- evalAloneSeq scopeRef args
  l <- getList (last args')
  call scopeRef (point first) pr (init args' ++ l)
soApply _        _                  = reportE' "at least two arguments required"

-- | Built-in function error
biError :: IORef Scope -> [SExpr] -> EvalM SExpr
biError _ [exp] = reportE' =<< getString exp
biError _ _     = reportE' "just one argument required"

specialOperators = [("let",      Nothing, soLet)
                   ,("set",      Just 2,  soSet)
                   ,("define",   Just 2,  soDefine)
                   ,("lambda",   Nothing, soLambda)
                   ,("def?",     Just 1,  soIsDef)
                   ,("undef",    Just 1,  soUndef)
                   ,("bind",     Nothing, soBind)
                   ,("apply",    Just 2,  soApply)
                   ,("type",     Just 1,  withEvaluatedArgs biType)
                   ,("error",    Just 1,  withEvaluatedArgs biError)]
