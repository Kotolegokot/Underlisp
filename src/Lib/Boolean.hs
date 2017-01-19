module Lib.Boolean (builtinFunctions
                   ,specialOperators) where

import Control.Monad (foldM)
import Control.Conditional (ifM, (<&&>), (<||>))
import Base
import Evaluator

default (Int)

biNot :: [SExpr] -> Eval SExpr
biNot [SAtom p (ABool b)] = return $ SAtom p (ABool $ not b)
biNot [other]             = report (point other) "boolean expected"
biNot _                   = reportUndef "just one argument required"

soAnd :: Env -> [SExpr] -> Eval (Env, SExpr)
soAnd e xs = do
  result <- foldM (\acc x -> return acc <&&> (getBool =<< snd <$> eval e x)) False xs
  return (e, bool result)

soOr :: Env -> [SExpr] -> Eval (Env, SExpr)
soOr e xs = do
  result <- foldM (\acc x -> return acc <||> (getBool =<< snd <$> eval e x)) True xs
  return (e, bool result)

soImpl :: Env -> [SExpr] -> Eval (Env, SExpr)
soImpl e [arg1, arg2] = do
  (_, exp1) <- eval e arg1
  result <- ifM (getBool exp1) (snd <$> eval e arg2) (return $ bool True)
  return (e, result)
soImpl _ _            = reportUndef "two arguments requried"

builtinFunctions = [("not", Just 1,  biNot)]
specialOperators = [("and", Nothing, soAnd)
                   ,("or",  Nothing, soOr)
                   ,("->",  Just 2,  soImpl)]
