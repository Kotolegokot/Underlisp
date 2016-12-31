{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Boolean (builtinNot,
                    spopAnd,
                    spopOr,
                    spopImpl) where

import Base
import Exception

builtinNot :: [SExpr] -> IO SExpr
builtinNot [sexpr] = return . bool . not . fromBool $ sexpr
builtinNot _       = reportUndef "just one argument required"

spopAnd :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopAnd eval evalScope context (x:xs) = do
  (_, expr) <- eval context x
  case fromBool expr of
    True  -> spopAnd eval evalScope context xs
    False -> return (context, bool False)
spopAnd _    _          context []     = return (context, bool True)

spopOr :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopOr eval evalScope context (x:xs) = do
  (_, expr) <- eval context x
  case fromBool expr of
    True  -> return (context, bool True)
    False -> spopOr eval evalScope context xs
spopOr _    _          context []     = return (context, bool False)

spopImpl :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopImpl eval _ context [arg1, arg2] = do
  (_, expr1) <- eval context arg1
  if not $ fromBool expr1
    then return (context, bool True)
    else eval context arg2
spopImpl _    _  _      _            = reportUndef "two arguments requried"
