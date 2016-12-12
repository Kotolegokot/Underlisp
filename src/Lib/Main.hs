module Lib.Main (spop_let,
                 spop_lambda,
                 spop_defvar,
                 builtin_type) where

import qualified Data.Map as Map
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
        func = UserDefinedFunction arg_names rest (SList $ SSymbol "seq" : body)
spop_lambda _    _       _            = error "lambda: at least one argument required"

-- special operator defvar
spop_defvar :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_defvar eval context [var, value]
  | not $ is_symbol var = error "first argument of 'defvar' must be a keyword"
  | otherwise           = do
      (expr, _) <- eval context value
      return (expr, Map.insert (from_symbol var) expr context)
spop_defvar _    _       _ = error "'defvar' requires two arguments"

-- built-in function type
builtin_type :: [SExpr] -> IO SExpr
builtin_type [sexpr] = return . SString $ show_type sexpr
builtin_type _       = error "'type' requires just one argument"

