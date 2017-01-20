module Lib.Main (builtinFunctions
                ,specialOperators) where

import Control.Monad.State
import Base
import Evaluator
import Type

default (Int)

-- | special operator lambda
-- (lambda lambda-list [body])
soLambda :: [SExpr] -> Lisp SExpr
soLambda (lambdaList:body) = do
  prototype <- parseLambdaList lambdaList
  current <- get
  return . procedure $ UserDefined current prototype body []
soLambda []                = reportE' "at least one argument expected"

-- special operator let
soLet :: [SExpr] -> Lisp SExpr
soLet (SList p pairs : body) = do
  modify passIn
  putBindings pairs
  exp <- evalBody body
  modify passOut
  return exp
    where putBindings :: [SExpr] -> Lisp ()
          putBindings = mapM_ putBinding

          putBinding :: SExpr -> Lisp ()
          putBinding (SList _ [SAtom _ (ASymbol var), value]) = do
            exp <- evalAlone value
            void . modify $ linsert var (EnvSExpr exp)
          putBinding (SList _ [exp1, _]) = reportE (point exp1) "first item in a binding pair must be a keyword"
          putBinding other               = reportE (point other) "(var value) pair expected"
soLet       [expr]               = reportE (point expr) "list expected"
soLet       _                    = reportE' "at least one argument expected"

soIsDef :: [SExpr] -> Lisp SExpr
soIsDef [sKey] = do
  key <- getSymbol =<< evalAlone sKey
  result <- gets $ memberSExpr key
  return $ bool result
soIsDef _      = reportE' "just one argument required"

soUndef :: [SExpr] -> Lisp SExpr
soUndef [sKey] = do
  key <- getSymbol =<< evalAlone sKey
  modify $ envDelete key
  return nil
soUndef _      = reportE' "just one argument required"

-- special operator define
soSet :: [SExpr] -> Lisp SExpr
soSet [sKey, sValue] = do
  key <- getSymbol =<< evalAlone sKey
  result <- gets $ lookupSExpr key
  case result of
    Just (SAtom _ (AProcedure SpecialOp {})) -> reportE' "rebinding special operators is forbidden"
    _                                        -> do
      value <- evalAlone sValue
      modify $ linsert key (EnvSExpr value)
      return nil
soSet _              = reportE' "two arguments required"

-- special operator mutate
soMutate :: [SExpr] -> Lisp SExpr
soMutate [sVar, sValue] = do
  key <- getSymbol =<< evalAlone sVar
  result <- gets $ lookupSExpr key
  case result of
    Just (SAtom _ (AProcedure SpecialOp {})) -> reportE' "rebinding special operators is forbidden"
    Just _                                          -> do
      value <- evalAlone sValue
      modify $ linsert key (EnvSExpr value)
      return nil
    Nothing                                         -> reportE' $ "undefined identificator '" ++ key ++ "'"
soMutate _               = reportE' "two arguments required"

-- built-in function type
biType :: [SExpr] -> Lisp SExpr
biType [exp] = return . symbol . showType $ exp
biType _     = reportE' "just one argument required"

soBind :: [SExpr] -> Lisp SExpr
soBind (first:args) = do
  pr <- getProcedure =<< evalAlone first
  case pr of
    so@SpecialOp {} -> procedure <$> bind so args
    other           -> do
      args' <- mapM evalAlone args
      procedure <$> bind other args'
soBind []            = reportE' "at least one argument required"

-- special operator apply
soApply :: [SExpr] -> Lisp SExpr
soApply (first:args@(_:_)) = do
  pr <- getProcedure =<< evalAlone first
  args' <- mapM evalAlone args
  l <- getList (last args')
  call (point first) pr (init args' ++ l)
soApply _             = reportE' "at least two arguments required"

-- built-in function error
biError :: [SExpr] -> Lisp SExpr
biError [exp] = reportE' =<< getString exp
biError _     = reportE' "just one argument required"

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
