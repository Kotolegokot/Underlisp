module Evaluator (evaluate) where

import qualified Data.Map as Map
import qualified Reader
import Control.Monad (void)
import SExpr
import Lib.Everything

evaluate :: SExpr -> IO ()
evaluate (SList (SKeyword "program":body)) = void $ eval_list eval_sexpr start_context body
evaluate (SList _)                         = error "program must start with calling 'program'"
evaluate _                                 = error "program must be a list"

eval_sexpr :: Context -> SExpr -> IO (SExpr, Context)
eval_sexpr context (SList (first:body)) = do
    (expr, new_context) <- eval_sexpr context first
    case expr of
      SFunc func@(UserDefined count_args fexpr) -> eval_sexpr new_context (apply func body)
      SFunc (BuiltIn _ f)                       -> f eval_sexpr new_context body
      _                                         -> error $ "can't execute s-expression: '" ++ show_sexpr expr ++ "'"
eval_sexpr context (SList [])           = error "can't execute empty list"
eval_sexpr context (SKeyword str) 
  | str `Map.member` context = return (context Map.! str, context)
  | otherwise                = error $ "undefined identificator '" ++ str ++ "'"
eval_sexpr context sexpr                = return (sexpr, context)

start_context :: Context
start_context = Map.fromList . fmap (\(name, f) -> (name, SFunc $ BuiltIn name f)) $ [
  ("let",          builtin_let),
  ("lambda",       builtin_lambda),
  ("define",       builtin_define),
  ("print",        builtin_print),
  ("print-ln",     builtin_print_ln),
  ("flush",        builtin_flush),
  ("get-line",     builtin_get_line),
  ("type",         builtin_type),
  ("if",           builtin_if),
  ("unless",       builtin_unless),
  ("=",            builtin_eq),
  ("/=",           builtin_ne),
  ("<",            builtin_lt),
  (">",            builtin_gt),
  ("<=",           builtin_le),
  (">=",           builtin_ge),
  ("not",          builtin_not),
  ("and",          builtin_and),
  ("or",           builtin_or),
  ("->",           builtin_impl),
  ("seq",          builtin_seq),
  ("+",            builtin_sum),
  ("-",            builtin_substract),
  ("*",            builtin_product),
  ("/",            builtin_divide),
  ("float",        builtin_float),
  ("concat",       builtin_concat),
  ("str-to-int",   builtin_str_to_int),
  ("str-to-float", builtin_str_to_float),
  ("list",         builtin_list),
  ("str-length",   builtin_str_length),
  ("head",         builtin_head),
  ("tail",         builtin_tail),
  ("init",         builtin_init),
  ("last",         builtin_last),
  ("length",       builtin_length),
  ("append",       builtin_append),
  ("nth",          builtin_nth),
  ("quote",        builtin_quote),
  ("interprete",   builtin_interprete),
  ("eval",         builtin_eval) ]
