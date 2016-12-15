module Lib.Main (spop_let,
                 spop_lambda,
                 spop_defvar,
                 builtin_type,
                 builtin_bind,
                 builtin_error ) where

import qualified Data.Map as Map
import Data.Char (toUpper)
import SExpr
import Lib.Internal

-- special operator let
spop_let :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_let eval context ((SList pairs):body) = do
    new_context <- handle_pairs pairs context
    eval new_context (SList (SSymbol "seq":body))
        where handle_pairs (x:xs) acc = case x of
                                          (SList [SSymbol var, value]) -> do
                                              (expr, _) <- eval acc value
                                              handle_pairs xs (Map.insert var expr acc)
                                          (SList [_, _]) -> error "first item in a let binding pair must be a keyword"
                                          _              -> error "a binding in 'let' must be of the following form: (var value)"
              handle_pairs []     acc = return acc
spop_let _    _       _               = error "list of bindings expected"

-- special operator lambda
spop_lambda :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_lambda eval context (lambda_list:body) = return (SCallable func, context)
  where (arg_names, rest) = handle_lambda_list lambda_list
        func = UserDefined (Map.keys context) arg_names rest (SList $ SSymbol "seq" : body) []
spop_lambda _    _       _            = error "lambda: at least one argument required"

-- special operator defvar
spop_defvar :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_defvar eval context [var, value]
  | not $ is_symbol var = error "first argument of 'defvar' must be a symbol"
  | otherwise           = do
      let new_context = Map.insert (from_symbol var) (SList []) context
      (expr, _) <- eval new_context value
      return (expr, Map.insert (from_symbol var) expr context)
spop_defvar _    _       _ = error "defvar: two arguments required"

-- built-in function type
builtin_type :: [SExpr] -> IO SExpr
builtin_type [sexpr] = return . SSymbol . map toUpper . show_type $ sexpr
builtin_type _       = error "type: just one argument required"

builtin_bind :: [SExpr] -> IO SExpr
builtin_bind (first:args) = return $ case first of
                                       SCallable callable -> SCallable $ bind callable args
                                       _                  -> error "bind: callable expected"
builtin_bind _            = error "bind: at least one argument required"

-- built-in function error
builtin_error :: [SExpr] -> IO SExpr
builtin_error [SString err] = error err
builtin_error [_]           = error "error: string expected"
builtin_error _             = error "error: just one argument required"
