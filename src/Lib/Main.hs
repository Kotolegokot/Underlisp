{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Main (spop_let
                , spop_define
                , spop_lambda
                , spop_defined
                , builtin_type
                , builtin_bind
                , builtin_error) where

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

-- | special operator lambda
-- (lambda lambda-list [body])
spop_lambda :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_lambda _ _ e (lambda_list:body) = return (e, callable $ UserDefined e prototype body [])
  where prototype = parse_lambda_list lambda_list
spop_lambda _    _ _ [] = report_undef "lambda: at least one argument expected"

-- | takes an s-list of the form (arg1 arg2... [&rest argLast])
-- | and constructs a Prototype
parse_lambda_list :: SExpr -> Prototype
parse_lambda_list (SList p lambda_list)
  | not $ all is_symbol lambda_list = report p "all items in a lambda list must be symbols"
  | length ixs > 1                  = report p "more than one &rest in a lambda list is forbidden"
  | rest && ix /= count - 2         = report p "&rest must be last but one"
  | otherwise                       = if rest
                                      then Prototype (delete "&rest" . map from_symbol $ lambda_list) rest
                                      else Prototype (map from_symbol $ lambda_list) rest
  where ixs   = elemIndices (symbol "&rest") lambda_list
        ix    = head ixs
        rest  = length ixs == 1
        count = length lambda_list
parse_lambda_list _ = report_undef "lambda list must be a list"

-- special operator let
spop_let :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_let eval eval_scope e ((SList p pairs):body) = do
  e' <- handle_pairs pairs e
  (_, expr) <- eval_scope e' body
  return (e, expr)
    where handle_pairs (x:xs) acc = case x of
            (SList _ [SAtom _ (ASymbol var), value]) -> do
              (_, expr) <- eval acc value
              handle_pairs xs (Env.linsert var expr acc)
            (SList _ [expr1, _]) -> report (point expr1) "let: first item in a binding pair must be a keyword"
            _                    -> report_undef "let: bindings must be of the following form: (var value)"
          handle_pairs []     acc = return acc
spop_let _    _          _       [expr]               = report (point expr) "let: list expected"
spop_let _    _          _       _                    = report_undef "let: at least one argument expected"

spop_defined :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_defined eval _ e [arg] = do
  (_, expr) <- eval e arg
  return $ case expr of
    SAtom _ (ASymbol s) -> (e, bool $ s `Env.member` e)
    _                   -> report (point expr) "defined?: symbol expected"
spop_defined _    _ _ _    = report_undef "defined?: just one argument required"

-- special operator define
spop_define :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_define eval _ e [var, s_value]
  | not $ is_symbol var = report (point var) "define: first argument must be a symbol"
  | otherwise           = do
      let key = from_symbol var
      (_, value) <- eval e s_value
      return (Env.linsert key value e, nil)
spop_define _    _           _       _ = report_undef "define: two arguments required"

-- built-in function type
builtin_type :: [SExpr] -> IO SExpr
builtin_type [sexpr] = return . symbol . map toUpper . expr_type $ sexpr
builtin_type _       = report_undef "type: just one argument required"

builtin_bind :: [SExpr] -> IO SExpr
builtin_bind (first:args) = return $ case first of
                                       SAtom _ (ACallable c) -> callable $ bind c args
                                       _                     -> report (point first) "bind: callable expected"
builtin_bind _            = report_undef "bind: at least one argument required"

-- built-in function error
builtin_error :: [SExpr] -> IO SExpr
builtin_error [SList p err] = report p (map from_char err)
builtin_error [sexpr]       = report (point sexpr) "error: string expected"
builtin_error _             = error "error: just one argument required"
