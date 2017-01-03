{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Boolean (builtinFunctions
                   ,specialOperators) where

import Base
import Exception

biNot :: [SExpr] -> IO SExpr
biNot [sexpr] = return . bool . not . fromBool $ sexpr
biNot _       = reportUndef "just one argument required"

soAnd :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soAnd eval evalScope context (x:xs) = do
  (_, expr) <- eval context x
  case fromBool expr of
    True  -> soAnd eval evalScope context xs
    False -> return (context, bool False)
soAnd _    _          context []     = return (context, bool True)

soOr :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soOr eval evalScope context (x:xs) = do
  (_, expr) <- eval context x
  case fromBool expr of
    True  -> return (context, bool True)
    False -> soOr eval evalScope context xs
soOr _    _          context []     = return (context, bool False)

soImpl :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soImpl eval _ context [arg1, arg2] = do
  (_, expr1) <- eval context arg1
  if not $ fromBool expr1
    then return (context, bool True)
    else eval context arg2
soImpl _    _  _      _            = reportUndef "two arguments requried"

builtinFunctions = [("not", Just (1 :: Int),  biNot)]
specialOperators = [("and", Nothing,          soAnd)
                   ,("or",  Nothing,          soOr)
                   ,("->",  Just (2 :: Int),  soImpl)]
