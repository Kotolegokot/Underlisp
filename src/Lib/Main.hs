module Lib.Main (spop_let,
                 spop_defvar,
                 builtin_type,
                 builtin_bind,
                 builtin_error ) where

import qualified Data.Map as Map
import Data.Char (toUpper)
import SExpr
import Lib.Internal

-- special operator let
spop_let :: Eval -> EvalScope -> Context -> [SExpr] -> IO (Context, SExpr)
spop_let eval eval_scope context ((SList pairs):body) = do
  context' <- handle_pairs pairs context
  eval_scope context' body
    where handle_pairs (x:xs) acc = case x of
            (SList [SSymbol var, value]) -> do
              (_, expr) <- eval acc value
              handle_pairs xs (Map.insert var expr acc)
            (SList [_, _]) -> error "let: first item in a binding pair must be a keyword"
            _              -> error "let: bindings must be of the following form: (var value)"
          handle_pairs []     acc = return acc
spop_let _    _          _       [_]                  = error "let: list expected"
spop_let _    _          _       _                    = error "let: at least one argument expected"

-- special operator defvar
spop_defvar :: Eval -> EvalScope -> Context -> [SExpr] -> IO (Context, SExpr)
spop_defvar eval eval_scope context [var, value]
  | not $ is_symbol var = error "defvar: first argument must be a symbol"
  | otherwise           = do
      let var_name = from_symbol var
      (_, value') <- eval context value
      return (Map.insert var_name value' context, nil)
spop_defvar _    _           _       _ = error "defvar: two arguments required"

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
