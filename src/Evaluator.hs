module Evaluator (evaluate_program, evalute_module) where

import qualified Data.Map as Map
import qualified Reader
import Control.Monad (void, foldM)
import SExpr
import Lib.Everything

evaluate_program :: [SExpr] -> IO ()
evaluate_program body = do
  prelude <- load_prelude
  void $ eval_list eval_sexpr prelude body

evalute_module :: [SExpr] -> IO Context
evalute_module body = do
  prelude <- load_prelude
  (_, context) <- foldM (\(_, prev_context) sexpr -> eval_sexpr prev_context sexpr) (empty_list, prelude) body
  return context

eval_sexpr :: Context -> SExpr -> IO (SExpr, Context)
eval_sexpr context (SList (first:args)) = do
  (expr, _) <- eval_sexpr context first
  case expr of
    SCallable (UserDefined arg_names rest sexpr bound) -> do
        pairs <- mapM (eval_sexpr context) args
        let f_context = handle_args arg_names rest (bound ++ fmap fst pairs)
        eval_sexpr (f_context `Map.union` context) sexpr
    SCallable (Macro arg_names rest sexpr bound) -> do
        let f_context = handle_args arg_names rest (bound ++ args)
        (expr, _) <- eval_sexpr (f_context `Map.union` context) sexpr
        eval_sexpr context expr
    SCallable (BuiltIn _ _ f bound)                       -> do
        pairs <- mapM (eval_sexpr context) args
        result <- f (bound ++ (fmap fst pairs))
        return (result, context)
    SCallable (SpecialOp _ _ f bound)                     -> f eval_sexpr context (bound ++ args)
    _                                         -> error $ "can't execute s-expression: '" ++ show_sexpr expr ++ "'"
eval_sexpr context (SList [])           = error "can't execute empty list"
eval_sexpr context (SSymbol str) 
  | str `Map.member` context = return (context Map.! str, context)
  | otherwise                = error $ "undefined identificator '" ++ str ++ "'"
eval_sexpr context sexpr                = return (sexpr, context)

handle_args :: [String] -> Bool -> [SExpr] -> Context
handle_args arg_names False args
  | length arg_names > length args = error "too little arguments"
  | length arg_names < length args = error "too many arguements"
  | otherwise                    = foldl (\context (name, value) -> Map.insert name value context) Map.empty (zip arg_names args)
handle_args arg_names True args
  | length arg_names > length args = error "too little arguments"
  | otherwise                      = let (left, right) = splitAt (length arg_names - 1) args
                                      in let args' = left ++ [SList right]
                                          in foldl (\context (name, value) -> Map.insert name value context) Map.empty (zip arg_names args')

load_prelude :: IO Context
load_prelude = do
  text <- readFile "stdlib/prelude.lisp"
  (_, context) <- eval_list_with_context eval_sexpr start_context $ Reader.read text
  return context

start_context :: Context
start_context = Map.fromList $
    (fmap (\(name, args, f) -> (name, SCallable $ SpecialOp name args f [])) [
    ("let",               Nothing, spop_let),
    ("lambda",            Nothing, spop_lambda),
    ("defvar",            Just 2,  spop_defvar),
    ("if",                Just 3,  spop_if),
    ("macro",             Nothing, spop_macro),
    ("macro-expand",      Just 1,  spop_macro_expand),
    ("quote",             Just 1,  spop_quote),
    ("backquote",         Just 1,  spop_backquote),
    ("interprete",        Just 1,  spop_interprete),
    ("eval",              Just 1,  spop_eval),
    ("and",               Nothing, spop_and),
    ("or",                Nothing, spop_or),
    ("->",                Just 2,  spop_impl),
    ("context",           Nothing, spop_context),
    ("load-context",      Just 1,  spop_load_context),
    ("current-context",   Just 0,  spop_current_context),
    ("context-from-file", Just 1,  spop_context_from_file),
    ("seq",               Nothing, spop_seq) ]) ++
  (fmap (\(name, args, f) -> (name, SCallable $ BuiltIn name args f [])) [
    ("type",         Just 1,  builtin_type),
    ("bind",         Nothing, builtin_bind),
    ("print",        Just 1,  builtin_print),
    ("print-ln",     Just 1,  builtin_print_ln),
    ("flush",        Just 0,  builtin_flush),
    ("get-line",     Just 0,  builtin_get_line),
    ("list",         Nothing, builtin_list),
    ("head",         Just 1,  builtin_head),
    ("tail",         Just 1,  builtin_tail),
    ("init",         Just 1,  builtin_init),
    ("last",         Just 1,  builtin_last),
    ("null",         Just 1,  builtin_null),
    ("append",       Just 2,  builtin_append),
    ("nth",          Just 2,  builtin_nth),
    ("+",            Nothing, builtin_sum),
    ("-",            Just 2,  builtin_substract),
    ("*",            Nothing, builtin_product),
    ("/",            Just 2,  builtin_divide),
    ("float",        Just 1,  builtin_float),
    ("not",          Just 1,  builtin_not),
    ("=",            Just 2,  builtin_eq),
    ("/=",           Just 2,  builtin_ne),
    ("<",            Just 2,  builtin_lt),
    (">",            Just 2,  builtin_gt),
    ("<=",           Just 2,  builtin_le),
    (">=",           Just 2,  builtin_ge),
    ("concat",       Nothing, builtin_concat),
    ("str-to-int",   Just 1,  builtin_str_to_int),
    ("str-to-float", Just 1,  builtin_str_to_float),
    ("str-length",   Just 1,  builtin_str_length) ])

spop_context_from_file :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_context_from_file eval context [args] = do
  (sexpr, _) <- eval context args
  case sexpr of
    SString filename -> do
      text <- readFile filename
      new_context <- evalute_module $ Reader.read text
      return (SContext new_context, context)
    _                -> error "context-from-file: string expected"
spop_context_from_file _    _       _      = error "context-from-file: just one argument required"
