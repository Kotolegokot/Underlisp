module Evaluator (evaluate) where

import qualified Data.Map as Map
import qualified Reader
import SExpr
import BuiltIn

evaluate :: SExpr -> IO ()
evaluate (SList (SKeyword "program":body)) = mapM_ (eval_sexpr start_context) body
evaluate _                                 = error "a program must start with calling 'program'"

eval_sexpr ::Context -> SExpr -> IO SExpr
eval_sexpr context (SList (first:body)) = do
    expr <- eval_sexpr context first
    case expr of
           SFunc (UserDefined count_args fexpr) -> eval_sexpr context (apply (from_func expr) body)
           SFunc (BuiltIn _ f)                  -> f eval_sexpr context body
           _                                    -> error $ "can't execute s-expression: '" ++ show_sexpr expr ++ "'"
eval_sexpr context (SList [])           = error "can't execute empty list"
eval_sexpr context (SKeyword str) 
  | str `Map.member` context = return $ context Map.! str
  | otherwise                = error $ "undefined identificator '" ++ str ++ "'"
eval_sexpr context sexpr                = return sexpr

start_context :: Context
start_context = Map.fromList [
  ("let",          SFunc $ BuiltIn "let"          builtin_let),
  ("lambda",       SFunc $ BuiltIn "lambda"       builtin_lambda),
  ("print",        SFunc $ BuiltIn "print"        builtin_print),
  ("print-ln",     SFunc $ BuiltIn "print-ln"     builtin_print_ln),
  ("flush",        SFunc $ BuiltIn "flush"        builtin_flush),
  ("get-line",     SFunc $ BuiltIn "get-line"     builtin_get_line),
  ("type",         SFunc $ BuiltIn "type"         builtin_type),
  ("if",           SFunc $ BuiltIn "if"           builtin_if),
  ("unless",       SFunc $ BuiltIn "unless"       builtin_unless),
  ("=",            SFunc $ BuiltIn "="            builtin_eq),
  ("/=",           SFunc $ BuiltIn "/="           builtin_ne),
  ("<",            SFunc $ BuiltIn "<"            builtin_lt),
  (">",            SFunc $ BuiltIn ">"            builtin_gt),
  ("<=",           SFunc $ BuiltIn "<="           builtin_le),
  (">=",           SFunc $ BuiltIn ">="           builtin_ge),
  ("not",          SFunc $ BuiltIn "not"          builtin_not),
  ("and",          SFunc $ BuiltIn "and"          builtin_and),
  ("or",           SFunc $ BuiltIn "or"           builtin_or),
  ("->",           SFunc $ BuiltIn "->"           builtin_impl),
  ("seq",          SFunc $ BuiltIn "seq"          builtin_seq),
  ("+",            SFunc $ BuiltIn "+"            builtin_sum),
  ("-",            SFunc $ BuiltIn "-"            builtin_substract),
  ("*",            SFunc $ BuiltIn "*"            builtin_product),
  ("/",            SFunc $ BuiltIn "/"            builtin_divide),
  ("float",        SFunc $ BuiltIn "float"        builtin_float),
  ("concat",       SFunc $ BuiltIn "concat"       builtin_concat),
  ("str-to-int",   SFunc $ BuiltIn "str-to-int"   builtin_str_to_int),
  ("str-to-float", SFunc $ BuiltIn "str-to-float" builtin_str_to_float),
  ("list",         SFunc $ BuiltIn "list"         builtin_list),
  ("str-length",   SFunc $ BuiltIn "str-length"   builtin_str_length),
  ("head",         SFunc $ BuiltIn "head"         builtin_head),
  ("tail",         SFunc $ BuiltIn "tail"         builtin_tail),
  ("init",         SFunc $ BuiltIn "init"         builtin_init),
  ("last",         SFunc $ BuiltIn "last"         builtin_last),
  ("length",       SFunc $ BuiltIn "length"       builtin_length),
  ("append",       SFunc $ BuiltIn "append"       builtin_append),
  ("nth",          SFunc $ BuiltIn "nth"          builtin_nth),
  ("quote",        SFunc $ BuiltIn "quote"        builtin_quote),
  ("interprete",   SFunc $ BuiltIn "interprete"   builtin_interprete),
  ("eval",         SFunc $ BuiltIn "eval"         builtin_eval) ]
