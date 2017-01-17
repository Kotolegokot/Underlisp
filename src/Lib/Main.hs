module Lib.Main (builtinFunctions
                ,specialOperators) where

import Data.List (delete, elemIndices)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Data.Char (toUpper)
import Base
import Evaluator
import Type

-- | special operator lambda
-- (lambda lambda-list [body])
soLambda :: Env -> [SExpr] -> Eval (Env, SExpr)
soLambda e (lambdaList:body) = do
  prototype <- parseLambdaList lambdaList
  return (e, procedure $ UserDefined e prototype body [])
soLambda _ []                = reportUndef "at least one argument expected"

-- special operator let
soLet :: Env -> [SExpr] -> Eval (Env, SExpr)
soLet e ((SList p pairs):body) = do
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
  (_, key) <- eval e sKey
  if not $ isSymbol key
    then report (point sKey) "symbol expected"
    else return (e, bool $ (fromSymbol key) `memberSExpr` e)
soIsDef _ _      = reportUndef "just one argument required"

soUndef :: Env -> [SExpr] -> Eval (Env, SExpr)
soUndef e [sKey] = do
  (_, key) <- eval e sKey
  if not $ isSymbol key
    then report (point sKey) "symbol expected"
    else return (envDelete (fromSymbol key) e, nil)
soUndef _ _      = reportUndef "just one argument required"

-- special operator define
soSet :: Env -> [SExpr] -> Eval (Env, SExpr)
soSet e [sVar, sValue] = do
  (_, var) <- eval e sVar
  if not $ isSymbol var
    then report (point sVar) "first argument must be a symbol"
    else do
      let key = fromSymbol var
      (_, value) <- eval e sValue
      return (linsert key (EnvSExpr value) e, nil)
soSet _ _              = reportUndef "two arguments required"

-- special operator mutate
soMutate :: Env -> [SExpr] -> Eval (Env, SExpr)
soMutate e [sVar, sValue] = do
  (_, var) <- eval e sVar
  let key = fromSymbol var
  if not $ isSymbol var
    then report (point sVar) "first argument must be a symbol"
    else case lookupSExpr key e of
           Just _  -> do
             (_, value) <- eval e sValue
             return (linsert key (EnvSExpr value) e, nil)
           Nothing -> reportUndef $ "undefined identificator '" ++ key ++ "'"
soMutate _ _               = reportUndef "two arguments required"

-- built-in function type
biType :: [SExpr] -> Eval SExpr
biType [sexpr] = return . symbol . showType $ sexpr
biType _       = reportUndef "just one argument required"

soBind :: Env -> [SExpr] -> Eval (Env, SExpr)
soBind e (first:args) = do
  (_, first') <- eval e first
  if not $ isProcedure first'
    then report (point first) "procedure expected"
    else case fromProcedure first' of
           so@(SpecialOp _ _ _ _) -> do
             p <- bind so args
             return (e, procedure p)
           other                  -> do
             pairs <- mapM (eval e) args
             let args' = map snd pairs
             p <- bind other args'
             return (e, procedure p)
soBind _ []            = reportUndef "at least one argument required"

-- special operator apply
soApply :: Env -> [SExpr] -> Eval (Env, SExpr)
soApply e [first, args] = do
  (_, first') <- eval e first
  (_, args')  <- eval e args
  if not $ isList args'
    then report (point args) "list expected"
    else call (point first) e (fromProcedure first') (fromList args')
soApply _ _             = reportUndef "two arguments required"

-- built-in function error
biError :: [SExpr] -> Eval SExpr
biError [sexpr]
  | not $ isString sexpr = report (point sexpr) "string expected"
  | otherwise            = reportUndef $ fromString sexpr
biError _             = reportUndef "just one argument required"

builtinFunctions = [("type",  Just (1 :: Int), biType)
                   ,("error", Just 1,           biError)]

specialOperators = [("let",      Nothing,         soLet)
                   ,("set",      Just (2 :: Int), soSet)
                   ,("mutate",   Just 2,          soMutate)
                   ,("lambda",   Nothing,         soLambda)
                   ,("def?",     Just 1,          soIsDef)
                   ,("undef",    Just 1,          soUndef)
                   ,("bind",     Nothing,         soBind)
                   ,("apply",    Just 2,          soApply)]
