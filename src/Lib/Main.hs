{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Main (spopLet
                , spopSet
                , spopLambda
                , spopDefined
                , builtinType
                , spopBind
                , spopApply
                , builtinError) where

import Data.List (delete, elemIndices)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Env
import Env (Env)
import Data.Char (toUpper)
import SExpr
import LexicalEnvironment
import Callable
import Exception
import LispShow
import Util

-- | special operator lambda
-- (lambda lambda-list [body])
spopLambda :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopLambda _ _ e (lambdaList:body) = return (e, callable $ UserDefined e prototype body [])
  where prototype = parseLambdaList lambdaList
spopLambda _    _ _ [] = reportUndef "at least one argument expected"

-- special operator let
spopLet :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopLet eval evalScope e ((SList p pairs):body) = do
  e' <- handlePairs pairs e
  (_, expr) <- evalScope e' body
  return (e, expr)
    where handlePairs (x:xs) acc = case x of
            (SList _ [SAtom _ (ASymbol var), value]) -> do
              (_, expr) <- eval acc value
              handlePairs xs (Env.linsert var expr acc)
            (SList _ [expr1, _]) -> report (point expr1) "first item in a binding pair must be a keyword"
            _                    -> report (point x) "(var value) pair expected"
          handlePairs []     acc = return acc
spopLet _    _          _       [expr]               = report (point expr) "list expected"
spopLet _    _          _       _                    = reportUndef "at least one argument expected"

spopDefined :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopDefined eval _ e [arg] = do
  (_, expr) <- eval e arg
  return $ case expr of
    SAtom _ (ASymbol s) -> (e, bool $ s `Env.member` e)
    _                   -> report (point expr) "symbol expected"
spopDefined _    _ _ _     = reportUndef "just one argument required"

-- special operator define
spopSet :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopSet eval _ e [var, sValue]
  | not $ isSymbol var = report (point var) "first argument must be a symbol"
  | otherwise           = do
      let key = fromSymbol var
      (_, value) <- eval e sValue
      return (Env.linsert key value e, nil)
spopSet _    _           _       _ = reportUndef "two arguments required"

-- built-in function type
builtinType :: [SExpr] -> IO SExpr
builtinType [sexpr] = return . symbol . map toUpper . exprType $ sexpr
builtinType _       = reportUndef "just one argument required"

spopBind :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopBind eval _ e (first:args) = do
  (_, first') <- eval e first
  if not $ isCallable first'
    then report (point first) "callable expected"
    else case fromCallable first' of
           m@(Macro _ _ _ _)      -> return (e, callable $ bind m args)
           so@(SpecialOp _ _ _ _) -> return (e, callable $ bind so args)
           other                  -> do
             pairs <- mapM (eval e) args
             let args' = map snd pairs
             return (e, callable $ bind other args')
spopBind _   _ _ []            = reportUndef "at least one argument required"

-- special operator apply
spopApply :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spopApply eval evalScope e [first, args] = do
  (_, first') <- eval e first
  (_, args')  <- eval e args
  if not $ isList args'
    then report (point args) "list expected"
    else call (point first) eval evalScope e (fromCallable first') (fromList args')
spopApply _    _ _ _             = reportUndef "two arguments required"

-- built-in function error
builtinError :: [SExpr] -> IO SExpr
builtinError [SList p err] = report p (map fromChar err)
builtinError [sexpr]       = report (point sexpr) "string expected"
builtinError _             = reportUndef "just one argument required"
