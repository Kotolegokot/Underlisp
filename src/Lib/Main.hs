{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Main (spop_let,
                 spop_defvar,
                 spop_lambda,
                 builtin_type,
                 builtin_bind,
                 builtin_error) where

import Data.List (delete, elemIndices)
import qualified Data.Map as Map
import qualified Env
import Env (Env)
import Data.Char (toUpper)
import SExpr
import LexicalEnvironment
import Callable

-- special operator lambda
spop_lambda :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_lambda _ _ e (lambda_list:body) = return (e, callable $ UserDefined e prototype body [])
  where prototype = parse_lambda_list lambda_list
spop_lambda _    _ _ [] = error "lambda: at least one argument expected"

-- | takes an s-list of the form (arg1 arg2... [&rst argLast])
-- | and constructs a Prototype
parse_lambda_list :: SExpr -> Prototype
parse_lambda_list (SList lambda_list)
  | not $ all is_symbol lambda_list = error "all items in a lambda list must be symbols"
  | length ixs > 1                  = error "more than one &rest in a lambda list is forbidden"
  | rest && ix /= count - 2         = error "&rest must be last but one"
  | otherwise                       = if rest
                                      then Prototype (delete "&rest" . map from_symbol $ lambda_list) rest
                                      else Prototype (map from_symbol $ lambda_list) rest
  where ixs   = elemIndices (symbol "&rest") lambda_list
        ix    = head ixs
        rest  = length ixs == 1
        count = length lambda_list
parse_lambda_list _ = error "lambda list must be a list"

-- special operator let
spop_let :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_let eval eval_scope e ((SList pairs):body) = do
  e' <- handle_pairs pairs e
  (_, expr) <- eval_scope e' body
  return (e, expr)
    where handle_pairs (x:xs) acc = case x of
            (SList [SAtom (ASymbol var), value]) -> do
              (_, expr) <- eval acc value
              handle_pairs xs (Env.linsert var expr acc)
            (SList [_, _]) -> error "let: first item in a binding pair must be a keyword"
            _              -> error "let: bindings must be of the following form: (var value)"
          handle_pairs []     acc = return acc
spop_let _    _          _       [_]                  = error "let: list expected"
spop_let _    _          _       _                    = error "let: at least one argument expected"

-- special operator defvar
spop_defvar :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_defvar eval eval_scope e [var, value]
  | not $ is_symbol var = error "defvar: first argument must be a symbol"
  | otherwise           = do
      let key = from_symbol var
      (_, value') <- eval e value
      return (Env.linsert key value' e, nil)
spop_defvar _    _           _       _ = error "defvar: two arguments required"

-- built-in function type
builtin_type :: [SExpr] -> IO SExpr
builtin_type [sexpr] = return . symbol . map toUpper . expr_type $ sexpr
builtin_type _       = error "type: just one argument required"

builtin_bind :: [SExpr] -> IO SExpr
builtin_bind (first:args) = return $ case first of
                                       SAtom (ACallable c) -> callable $ bind c args
                                       _           -> error "bind: callable expected"
builtin_bind _            = error "bind: at least one argument required"

-- built-in function error
builtin_error :: [SExpr] -> IO SExpr
builtin_error [SAtom (AString err)] = error err
builtin_error [_]                   = error "error: string expected"
builtin_error _                     = error "error: just one argument required"
