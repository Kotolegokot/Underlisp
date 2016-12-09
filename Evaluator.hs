module Evaluator (evaluate) where

import qualified Context
import qualified Data.Map as Map
import SExpr

evaluate :: SExpr -> IO ()
evaluate (SList (SKeyword "program":body)) = mapM_ (eval_sexpr Context.empty) body
evaluate _                                 = error "a program must start with calling 'program'"

eval_sexpr :: Context.Context -> SExpr -> IO SExpr
eval_sexpr context (SList (SKeyword fname:body)) = call_function context fname body
eval_sexpr _       (SList _) = error "keyword expected"
eval_sexpr context (SKeyword kword)
  | kword `Map.member` context = return $ context Map.! kword
  | otherwise                  = error $ "undefined indentificator '" ++ kword ++ "'"

call_function :: Context.Context -> String -> [SExpr] -> IO SExpr
call_function _ _ _ = return empty_list
