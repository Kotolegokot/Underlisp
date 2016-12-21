module Lib.Main (spop_let,
                 spop_defvar,
                 builtin_type,
                 builtin_bind,
                 builtin_error ) where

import qualified Data.Map as Map
import qualified Env
import Env (Env)
import Data.Char (toUpper)
import SExpr
import Callable

-- special operator let
spop_let :: Eval SExpr -> EvalScope SExpr -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_let eval eval_scope env ((SList pairs):body) = do
  env' <- handle_pairs pairs env
  eval_scope env' body
    where handle_pairs (x:xs) acc = case x of
            (SList [SAtom (ASymbol var), value]) -> do
              (_, expr) <- eval acc value
              handle_pairs xs (Env.insert var expr acc)
            (SList [_, _]) -> error "let: first item in a binding pair must be a keyword"
            _              -> error "let: bindings must be of the following form: (var value)"
          handle_pairs []     acc = return acc
spop_let _    _          _       [_]                  = error "let: list expected"
spop_let _    _          _       _                    = error "let: at least one argument expected"

-- special operator defvar
spop_defvar :: Eval SExpr -> EvalScope SExpr -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_defvar eval eval_scope env [var, value]
  | not $ is_symbol var = error "defvar: first argument must be a symbol"
  | otherwise           = do
      let key = from_symbol var
      (_, value') <- eval env value
      return (Env.insert key value' env, nil)
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
