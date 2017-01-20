module Lib.Main (builtinFunctions
                ,specialOperators) where

import Base
import Evaluator
import Type

default (Int)

-- | special operator lambda
-- (lambda lambda-list [body])
soLambda :: Env -> [SExpr] -> Eval (Env, SExpr)
soLambda e (lambdaList:body) = do
  prototype <- parseLambdaList lambdaList
  return (e, procedure $ UserDefined e prototype body [])
soLambda _ []                = reportUndef "at least one argument expected"

-- special operator let
soLet :: Env -> [SExpr] -> Eval (Env, SExpr)
soLet e (SList p pairs : body) = do
  e' <- handlePairs pairs (pass e)
  (_, expr) <- evalScope e' body
  return (e, expr)
    where handlePairs (x:xs) acc = case x of
            (SList _ [SAtom _ (ASymbol var), value]) -> do
              (_, expr) <- eval acc value
              handlePairs xs (linsert var (EnvSExpr expr) acc)
            (SList _ [expr1, _]) -> report (point expr1) "first item in a binding pair must be a keyword"
            _                    -> report (point x) "(var value) pair expected"
          handlePairs []     acc = return acc
soLet _       [expr]               = report (point expr) "list expected"
soLet _       _                    = reportUndef "at least one argument expected"

soIsDef :: Env -> [SExpr] -> Eval (Env, SExpr)
soIsDef e [sKey] = do
  key <- getSymbol =<< snd <$> eval e sKey
  return (e, bool $ key `memberSExpr` e)
soIsDef _ _      = reportUndef "just one argument required"

soUndef :: Env -> [SExpr] -> Eval (Env, SExpr)
soUndef e [sKey] = do
  key <- getSymbol =<< snd <$> eval e sKey
  return (envDelete key e, nil)
soUndef _ _      = reportUndef "just one argument required"

-- special operator define
soSet :: Env -> [SExpr] -> Eval (Env, SExpr)
soSet e [sKey, sValue] = do
  key <- getSymbol =<< snd <$> eval e sKey
  case lookupSExpr key e of
    Just (SAtom _ (AProcedure SpecialOp {})) -> reportUndef "rebinding special operators is forbidden"
    _                                               -> do
      (_, value) <- eval e sValue
      return (linsert key (EnvSExpr value) e, nil)
soSet _ _              = reportUndef "two arguments required"

-- special operator mutate
soMutate :: Env -> [SExpr] -> Eval (Env, SExpr)
soMutate e [sVar, sValue] = do
  key <- getSymbol =<< snd <$> eval e sVar
  case lookupSExpr key e of
    Just (SAtom _ (AProcedure SpecialOp {})) -> reportUndef "rebinding special operators is forbidden"
    Just _                                          -> do
      (_, value) <- eval e sValue
      return (linsert key (EnvSExpr value) e, nil)
    Nothing                                         -> reportUndef $ "undefined identificator '" ++ key ++ "'"
soMutate _ _               = reportUndef "two arguments required"

-- built-in function type
biType :: [SExpr] -> Eval SExpr
biType [exp] = return . symbol . showType $ exp
biType _     = reportUndef "just one argument required"

soBind :: Env -> [SExpr] -> Eval (Env, SExpr)
soBind e (first:args) = do
  pr <- getProcedure =<< snd <$> eval e first
  case pr of
    so@SpecialOp {} -> do
      p <- bind so args
      return (e, procedure p)
    other           -> do
      args' <- mapM ((snd <$>) . eval e) args
      p <- bind other args'
      return (e, procedure p)
soBind _ []            = reportUndef "at least one argument required"

-- special operator apply
soApply :: Env -> [SExpr] -> Eval (Env, SExpr)
soApply e (first:args@(_:_)) = do
  pr <- getProcedure =<< snd <$> eval e first
  args' <- mapM ((snd <$>) . eval e) args
  l <- getList (last args')
  call (point first) e pr (init args' ++ l)
soApply _ _             = reportUndef "at least two arguments required"

-- built-in function error
biError :: [SExpr] -> Eval SExpr
biError [exp] = reportUndef =<< getString exp
biError _     = reportUndef "just one argument required"

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
