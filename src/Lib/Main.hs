module Lib.Main (builtinFunctions
                ,specialOperators) where

-- map
import qualified Data.Map as Map
import Data.Map (Map)

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
soLambda :: IORef Scope -> [SExpr] -> Lisp SExpr
soLambda scopeRef (lambdaList:body) = do
  prototype <- parseLambdaList lambdaList
  return . procedure $ UserDefined scopeRef prototype body []
soLambda _        []                = reportE' "at least one argument expected"

-- | Special operator let
soLet :: IORef Scope -> [SExpr] -> Lisp SExpr
soLet scopeRef (SList p pairs : body) = do
  bindings <- getBindings scopeRef pairs
  childScope <- liftIO $ newLocal' bindings scopeRef
  evalBody childScope body
    where getBindings :: IORef Scope -> [SExpr] -> Lisp (Map String Binding)
          getBindings scopeRef = fmap Map.fromList . mapM (getBinding scopeRef)

          getBinding :: IORef Scope -> SExpr -> Lisp (String, Binding)
          getBinding scopeRef (SList _ [SAtom _ (ASymbol var), value]) = do
            exp <- evalAlone scopeRef value
            return (var, BSExpr exp)
          getBinding _        (SList _ [exp1, _]) = reportE (point exp1) "first item in a binding pair must be a keyword"
          getBinding _        other               = reportE (point other) "(var value) pair expected"
soLet _        [expr]                    = reportE (point expr) "list expected"
soLet _        _                         = reportE' "at least one argument expected"

soIsDef :: IORef Scope -> [SExpr] -> Lisp SExpr
soIsDef scopeRef [sKey] = do
  key <- getSymbol =<< evalAlone scopeRef sKey
  liftIO $ bool <$> exploreIORefIO scopeRef (scMemberS key)
soIsDef _        _      = reportE' "just one argument required"

soUndef :: IORef Scope -> [SExpr] -> Lisp SExpr
soUndef scopeRef [sKey] = do
  key <- getSymbol =<< evalAlone scopeRef sKey
  liftIO $ modifyIORefIO scopeRef (scDelete key)
  return nil
soUndef _        _      = reportE' "just one argument required"

-- special operator define
soSet :: IORef Scope -> [SExpr] -> Lisp SExpr
soSet scopeRef [sKey, sValue] = do
  key <- getSymbol =<< evalAlone scopeRef sKey
  result <- liftIO $ exploreIORefIO scopeRef (scLookupS key)
  case result of
    Just (SAtom _ (AProcedure SpecialOp {})) -> reportE' "rebinding special operators is forbidden"
    _                                        -> do
      value <- evalAlone scopeRef sValue
      liftIO $ modifyIORefIO scopeRef (scSet key $ BSExpr value)
      return nil
soSet _        _              = reportE' "two arguments required"

-- special operator mutate
soMutate :: IORef Scope -> [SExpr] -> Lisp SExpr
soMutate scopeRef [sVar, sValue] = do
  key <- getSymbol =<< evalAlone scopeRef sVar
  result <- liftIO $ exploreIORefIO scopeRef (scLookupS key)
  case result of
    Just (SAtom _ (AProcedure SpecialOp {})) -> reportE' "rebinding special operators is forbidden"
    Just _                                   -> do
      value <- evalAlone scopeRef sValue
      liftIO $ modifyIORefIO scopeRef (scSet key $ BSExpr value)
      return nil
    Nothing                                  -> reportE' $ "undefined identificator '" ++ key ++ "'"
soMutate _        _              = reportE' "two arguments required"

-- | Built-in function type
biType :: IORef Scope -> [SExpr] -> Lisp SExpr
biType _ [exp] = return . symbol . showType $ exp
biType _ _     = reportE' "just one argument required"

soBind :: IORef Scope -> [SExpr] -> Lisp SExpr
soBind scopeRef (first:args) = do
  pr <- getProcedure =<< evalAlone scopeRef first
  case pr of
    so@SpecialOp {} -> procedure <$> bind so args
    other           -> do
      args' <- evalAloneSeq scopeRef args
      procedure <$> bind other args'
soBind _        []            = reportE' "at least one argument required"

-- | Special operator apply
soApply :: IORef Scope -> [SExpr] -> Lisp SExpr
soApply scopeRef (first:args@(_:_)) = do
  pr <- getProcedure =<< evalAlone scopeRef first
  args' <- evalAloneSeq scopeRef args
  l <- getList (last args')
  call scopeRef (point first) pr (init args' ++ l)
soApply _        _                  = reportE' "at least two arguments required"

-- | Built-in function error
biError :: IORef Scope -> [SExpr] -> Lisp SExpr
biError _ [exp] = reportE' =<< getString exp
biError _ _     = reportE' "just one argument required"

builtinFunctions = [("type",  Just 1, biType)
                   ,("error", Just 1, biError)]

specialOperators = [("let",      Nothing, soLet)
                   ,("set",      Just 2,  soSet)
                   ,("mutate",   Just 2,  soMutate)
                   ,("lambda",   Nothing, soLambda)
                   ,("def?",     Just 1,  soIsDef)
                   ,("undef",    Just 1,  soUndef)
                   ,("bind",     Nothing, soBind)
                   ,("apply",    Just 2,  soApply)]
