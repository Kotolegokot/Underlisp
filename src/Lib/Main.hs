module Lib.Main (builtinFunctions
                ,specialOperators) where

import Data.List (delete, elemIndices)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import Data.Char (toUpper)
import Base
import Exception
import Util
import Type

-- | special operator lambda
-- (lambda lambda-list [body])
soLambda :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soLambda _ _ e (lambdaList:body) = return (e, procedure $ UserDefined e prototype body [])
  where prototype = parseLambdaList lambdaList
soLambda _    _ _ [] = reportUndef "at least one argument expected"

-- special operator let
soLet :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soLet eval evalScope e ((SList p pairs):body) = do
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
soLet _    _          _       [expr]               = report (point expr) "list expected"
soLet _    _          _       _                    = reportUndef "at least one argument expected"

soIsDefined :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soIsDefined eval _ e [arg] = do
  (_, expr) <- eval e arg
  return $ case expr of
    SAtom _ (ASymbol s) -> (e, bool $ s `memberSExpr` e)
    _                   -> report (point expr) "symbol expected"
soIsDefined _    _ _ _     = reportUndef "just one argument required"

-- special operator define
soSet :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soSet eval _ e [var, sValue]
  | not $ isSymbol var = report (point var) "first argument must be a symbol"
  | otherwise           = do
      let key = fromSymbol var
      (_, value) <- eval e sValue
      return (linsert key (EnvSExpr value) e, nil)
soSet _    _           _       _ = reportUndef "two arguments required"

-- built-in function type
biType :: [SExpr] -> IO SExpr
biType [sexpr] = return . symbol . map toUpper . showType $ sexpr
biType _       = reportUndef "just one argument required"

soBind :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soBind eval _ e (first:args) = do
  (_, first') <- eval e first
  if not $ isProcedure first'
    then report (point first) "procedure expected"
    else case fromProcedure first' of
           so@(SpecialOp _ _ _ _) -> return (e, procedure $ bind so args)
           other                  -> do
             pairs <- mapM (eval e) args
             let args' = map snd pairs
             return (e, procedure $ bind other args')
soBind _   _ _ []            = reportUndef "at least one argument required"

-- special operator apply
soApply :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soApply eval evalScope e [first, args] = do
  (_, first') <- eval e first
  (_, args')  <- eval e args
  if not $ isList args'
    then report (point args) "list expected"
    else call (point first) eval evalScope e (fromProcedure first') (fromList args')
soApply _    _ _ _             = reportUndef "two arguments required"

-- built-in function error
biError :: [SExpr] -> IO SExpr
biError [sexpr]
  | not $ isString sexpr = report (point sexpr) "string expected"
  | otherwise            = reportUndef $ fromString sexpr
biError _             = reportUndef "just one argument required"

builtinFunctions = [("type",  Just (1 :: Int), biType)
                   ,("error", Just 1,           biError)]

specialOperators = [("let",      Nothing,         soLet)
                   ,("set",      Just (2 :: Int), soSet)
                   ,("lambda",   Nothing,         soLambda)
                   ,("defined?", Just 1,          soIsDefined)
                   ,("bind",     Nothing,         soBind)
                   ,("apply",    Just 2,          soApply)]
