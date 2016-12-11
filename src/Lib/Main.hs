module Lib.Main (spop_let,
                 spop_lambda,
                 spop_defvar,
                 spop_define,
                 builtin_type) where

import qualified Data.Map as Map
import Data.List (elemIndex, elemIndices, delete)
import Expr
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
    where (args, args_list) = handle_lambda_list first
          func = UserDefinedFunction args (FList $ FKeyword "seq" : fmap handle_sexpr body)
          handle_sexpr (SList list)   = FList . fmap handle_sexpr $ list
          handle_sexpr (SSymbol str) = case elemIndex str args_list of
                                                Just index -> FRef index
                                                Nothing    -> FKeyword str
          handle_sexpr sexpr                = sexpr2fexpr sexpr
spop_lambda _    _       _     = error "'define' requires at least one argument"

handle_lambda_list :: SExpr -> (Args, [String])
handle_lambda_list (SList lambda_list)
  | not $ all is_keyword lambda_list = error "every item in lambda list must be a keyword"
  | length ixs > 1                   = error "more than one &rest in lambda list"
  | rest && ix /= count - 2          = error "&rest must be last but one"
  | otherwise                        = if rest
                                          then (Args (count - 2) rest, delete "&rest" . fmap from_keyword $ lambda_list)
                                          else (Args count rest, fmap from_keyword lambda_list)
    where ixs   = elemIndices (SSymbol "&rest") lambda_list
          ix    = head ixs
          rest  = length ixs == 1
          count = length lambda_list
handle_lambda_list _                 = error "lambda list must be a list"

-- special operator defvar
spop_defvar :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_defvar eval context [var, value]
  | not $ is_keyword var = error "first argument of 'defvar' must be a keyword"
  | otherwise            = do
      (expr, _) <- eval context value
      return (expr, Map.insert (from_keyword var) expr context)
spop_defvar _    _       _ = error "'defvar' requires two arguments"

-- special operator define
-- (must be reimplemented as a macro later)
spop_define :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_define eval context (name:rest) = eval context (SList [SSymbol "defvar", name, SList ([SSymbol "lambda"] ++ rest)])

-- built-in function type
builtin_type :: [SExpr] -> IO SExpr
builtin_type [sexpr] = return . SString $ show_type sexpr
builtin_type _       = error "'type' requires just one argument"

