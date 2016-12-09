module Evaluator (evaluate) where

import System.IO

import qualified Context
import qualified Data.Map as Map
import SExpr

evaluate :: SExpr -> IO ()
evaluate (SList (SKeyword "program":body)) = mapM_ (eval_sexpr Context.empty) body
evaluate _                                 = error "a program must start with calling 'program'"

eval_sexpr :: Context.Context -> SExpr -> IO SExpr
eval_sexpr context (SList (SKeyword fname:body)) = call_function fname context body
eval_sexpr _       (SList _) = error "keyword expected"
eval_sexpr context (SKeyword kword)
  | kword `Map.member` context = return $ context Map.! kword
  | otherwise                  = error $ "undefined indentificator '" ++ kword ++ "'"

call_function :: String -> Context.Context -> [SExpr] -> IO SExpr
call_function fname = case fname of
                        "let"          -> builtin_let
                        "print"        -> builtin_print
                        "print-ln"     -> builtin_print_ln
                        "flush"        -> builtin_flush
                        "get-line"     -> builtin_get_line
                        "type"         -> builtin_type
                        "if"           -> builtin_if
                        "unless"       -> builtin_unless
                        "="            -> builtin_eq
                        "/="           -> builtin_ne
                        "<"            -> builtin_lt
                        ">"            -> builtin_gt
                        "<="           -> builtin_le
                        ">="           -> builtin_ge
                        "not"          -> builtin_not
                        "&"            -> builtin_and
                        "|"            -> builtin_or
                        "->"           -> builtin_impl
                        "seq"          -> builtin_seq
                        "+"            -> builtin_sum
                        "-"            -> builtin_substract
                        "*"            -> builtin_product
                        "/"            -> builtin_divide
                        "float"        -> builtin_float
                        "concat"       -> builtin_concat
                        "str-to-int"   -> builtin_str_to_int
                        "str-to-float" -> builtin_str_to_float
                        "list"         -> builtin_list

--builtin_let

builtin_print :: Context.Context -> [SExpr] -> IO SExpr
builtin_print context [sexpr] = eval_sexpr context sexpr >>= (putStr . show_sexpr) >> return empty_list
builtin_print _       _       = error "'print' requires only one argument"

builtin_print_ln :: Context.Context -> [SExpr] -> IO SExpr
builtin_print_ln context [sexpr] = eval_sexpr context sexpr >>= (putStrLn . show_sexpr) >> return empty_list
builtin_print_ln _       _       = error "'print-ln' requires only one argument"

builtin_flush :: Context.Context -> [SExpr] -> IO SExpr
builtin_flush context [] = hFlush stdout >> return empty_list
builtin_flush _       _  = error "'flush' requires no arguments"

builtin_get_line :: Context.Context -> [SExpr] -> IO SExpr
builtin_get_line context [] = getLine >>= (return . SString)
builtin_get_line _       _  = error "'get-line' requires no arguments"

builtin_type :: Context.Context -> [SExpr] -> IO SExpr
builtin_type context [sexpr] = eval_sexpr context sexpr >>= (return . SString . showType)
builtin_type _       _       = error "'type' requires no arguments"

builtin_if :: Context.Context -> [SExpr] -> IO SExpr
builtin_if context [cond_sexpr]                          = builtin_if context [cond_sexpr, empty_list, empty_list]
builtin_if context [cond_sexpr, true_sexpr]              = builtin_if context [cond_sexpr, true_sexpr, empty_list]
builtin_if context [cond_sexpr, true_sexpr, false_sexpr] = do
    cond <- eval_sexpr context cond_sexpr
    if from_bool cond
       then eval_sexpr context true_sexpr
       else eval_sexpr context false_sexpr
builtin_if _       _                                     = error "'if' requires 1 to 3 arguments"
