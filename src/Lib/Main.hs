module Lib.Main (spop_let,
                 spop_lambda,
                 spop_defvar,
                 spop_define,
                 builtin_type) where

import qualified Data.Map as Map
import Data.List (elemIndex, elemIndices, delete)
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
spop_lambda eval context (first:body) = return (SCallable func, context)
  where (arg_names, rest) = handle_lambda_list first
        func = UserDefinedFunction' arg_names rest (SList $ SSymbol "seq" : body)
spop_lambda _    _       _            = error "lambda: at least one argument required"

handle_lambda_list :: SExpr -> ([String], Bool)
handle_lambda_list (SList lambda_list)
  | not $ all is_symbol lambda_list = error "every item in lambda list must be a keyword"
  | length ixs > 1                  = error "more than one &rest in lambda list is forbidden"
  | rest && ix /= count - 2         = error "&rest must be last but one"
  | otherwise                       = if rest then (delete "&rest" . map from_symbol $ lambda_list, rest)
                                              else (map from_symbol $ lambda_list, rest)
  where ixs   = elemIndices (SSymbol "&rest") lambda_list
        ix    = head ixs
        rest  = length ixs == 1
        count = length lambda_list
handle_lambda_list' _ = error "lambda list must be a list"

-- special operator defvar
spop_defvar :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_defvar eval context [var, value]
  | not $ is_symbol var = error "first argument of 'defvar' must be a keyword"
  | otherwise           = do
      (expr, _) <- eval context value
      return (expr, Map.insert (from_symbol var) expr context)
spop_defvar _    _       _ = error "'defvar' requires two arguments"

-- special operator define
-- (must be reimplemented as a macro later)
spop_define :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_define eval context (name:rest) = eval context (SList [SSymbol "defvar", name, SList ([SSymbol "lambda"] ++ rest)])

-- built-in function type
builtin_type :: [SExpr] -> IO SExpr
builtin_type [sexpr] = return . SString $ show_type sexpr
builtin_type _       = error "'type' requires just one argument"

