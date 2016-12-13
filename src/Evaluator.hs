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
      SCallable (UserDefinedFunction arg_names rest sexpr) -> do
          pairs <- mapM (eval_sexpr context) args
          let f_context = handle_args arg_names rest (fmap fst pairs)
          eval_sexpr (f_context `Map.union` context) sexpr
      SCallable (Macro arg_names rest sexpr) -> do
          let f_context = handle_args arg_names rest args
          (expr, _) <- eval_sexpr (f_context `Map.union` context) sexpr
          eval_sexpr context expr
      SCallable (BuiltInFunction _ f)                       -> do
          pairs <- mapM (eval_sexpr context) args
          result <- f (fmap fst pairs)
          return (result, context)
      SCallable (SpecialOperator _ f)                       -> f eval_sexpr context args
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
  text <- readFile "examples/prelude.lisp"
  (_, context) <- eval_list_with_context eval_sexpr start_context $ Reader.read text
  return context

start_context :: Context
start_context = Map.fromList $
  (fmap (\(name, f) -> (name, SCallable $ SpecialOperator name f)) [
    ("let",               spop_let),
    ("lambda",            spop_lambda),
    ("defvar",            spop_defvar),
    ("if",                spop_if),
    ("macro",             spop_macro),
    ("macro-expand",      spop_macro_expand),
    ("quote",             spop_quote),
    ("backquote",         spop_backquote),
    ("interprete",        spop_interprete),
    ("eval",              spop_eval),
    ("and",               spop_and),
    ("or",                spop_or),
    ("->",                spop_impl),
    ("context",           spop_context),
    ("load-context",      spop_load_context),
    ("current-context",   spop_current_context),
    ("context-from-file", spop_context_from_file),
    ("seq",               spop_seq) ]) ++
  (fmap (\(name, f) -> (name, SCallable $ BuiltInFunction name f)) [
    ("type",         builtin_type),
    ("print",        builtin_print),
    ("print-ln",     builtin_print_ln),
    ("flush",        builtin_flush),
    ("get-line",     builtin_get_line),
    ("list",         builtin_list),
    ("head",         builtin_head),
    ("tail",         builtin_tail),
    ("init",         builtin_init),
    ("last",         builtin_last),
    ("length",       builtin_length),
    ("append",       builtin_append),
    ("nth",          builtin_nth),
    ("+",            builtin_sum),
    ("-",            builtin_substract),
    ("*",            builtin_product),
    ("/",            builtin_divide),
    ("float",        builtin_float),
    ("not",          builtin_not),
    ("=",            builtin_eq),
    ("/=",           builtin_ne),
    ("<",            builtin_lt),
    (">",            builtin_gt),
    ("<=",           builtin_le),
    (">=",           builtin_ge),
    ("concat",       builtin_concat),
    ("str-to-int",   builtin_str_to_int),
    ("str-to-float", builtin_str_to_float),
    ("str-length",   builtin_str_length) ])

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
