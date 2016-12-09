module Evaluator (evaluate) where

import System.IO
import Text.Read
import qualified Data.Map as Map

import qualified Reader
import SExpr
import BuiltIn

start_context :: Context
start_context = Map.fromList [
  ("let",      SFunc $ BuiltIn "let"      builtin_let),
  ("print",    SFunc $ BuiltIn "print"    builtin_print),
  ("print-ln", SFunc $ BuiltIn "print-ln" builtin_print_ln),
  ("flush",    SFunc $ BuiltIn "flush"    builtin_flush),
  ("get-line", SFunc $ BuiltIn "get-line" builtin_get_line) ]

evaluate :: SExpr -> IO ()
evaluate (SList (SKeyword "program":body)) = mapM_ (eval_sexpr start_context) body
evaluate _                                 = error "a program must start with calling 'program'"

eval_sexpr ::Context -> SExpr -> IO SExpr
eval_sexpr context (SList (first:body)) = do
    expr <- eval_sexpr context first
    case expr of
           SFunc (UserDefined count_args fexpr) -> eval_sexpr context (apply (from_func expr) body)
           SFunc (BuiltIn _ f)                  -> f eval_sexpr context body
           _                                    -> error $ "can't execute sexpr: '" ++ show_sexpr expr ++ "'"
eval_sexpr context (SList [])           = error "can't execute empty list"
eval_sexpr context (SKeyword str) 
  | str `Map.member` context = return $ context Map.! str
  | otherwise                = error $ "undefined identificator '" ++ str ++ "'"
eval_sexpr context sexpr                = return sexpr
