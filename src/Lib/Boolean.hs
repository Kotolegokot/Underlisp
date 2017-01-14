{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Boolean (builtinFunctions
                   ,specialOperators) where

import Base
import Exception
import Util

biNot :: [SExpr] -> IO SExpr
biNot [sexpr] = return . bool . not . fromBool $ sexpr
biNot _       = reportUndef "just one argument required"

soAnd :: Env -> [SExpr] -> IO (Env, SExpr)
soAnd e (x:xs) = do
  (_, expr) <- eval e x
  case fromBool expr of
    True  -> soAnd e xs
    False -> return (e, bool False)
soAnd e []     = return (e, bool True)

soOr :: Env -> [SExpr] -> IO (Env, SExpr)
soOr e (x:xs) = do
  (_, expr) <- eval e x
  case fromBool expr of
    True  -> return (e, bool True)
    False -> soOr e xs
soOr e []     = return (e, bool False)

soImpl :: Env -> [SExpr] -> IO (Env, SExpr)
soImpl e [arg1, arg2] = do
  (_, expr1) <- eval e arg1
  if not $ fromBool expr1
    then return (e, bool True)
    else eval e arg2
soImpl _ _            = reportUndef "two arguments requried"

builtinFunctions = [("not", Just (1 :: Int),  biNot)]
specialOperators = [("and", Nothing,          soAnd)
                   ,("or",  Nothing,          soOr)
                   ,("->",  Just (2 :: Int),  soImpl)]
