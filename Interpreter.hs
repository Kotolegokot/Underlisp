module Interpreter (interprete, evaluate) where

import Control.Monad
import Data.Tree
import qualified Data.Map as Map
import Program
import SemanticAnalyzer
import BuiltInFunctions

evaluate :: Program -> IO ()
evaluate program@(Program functions body) = void $ eval_function functions body

interprete :: Tree Terminal -> Program
interprete (Node (TKeyword "program") body) = Program Map.empty $ head body
interprete _ = error "no 'program' at the beginning of the outer list"

eval_function :: Map.Map String Function -> Tree Terminal -> IO Terminal
eval_function functions (Node head args)
  | isKeyword head = call_function functions (fromKeyword head) =<< mapM (eval_function functions) args
  | otherwise      = if null args then return head else error "too many arguments"

-- built-in functions
call_function :: Map.Map String Function -> String -> [Terminal] -> IO Terminal

call_function _ "print"    args = print_ args
call_function _ "print-ln" args = print_ln_ args
call_function _ "type"     args = type_ args
call_function _ "+"        args = add_ args
call_function _ "-"        args = substract_ args
call_function _ "*"        args = product_ args
call_function _ "/"        args = divide_ args
call_function _ "="        args = equal_ args
call_function _ "/="       args = inequal_ args
call_function _ "<"        args = lt_ args
call_function _ ">"        args = gt_ args
call_function _ "<="       args = le_ args
call_function _ ">="       args = ge_ args

call_function _ func _ = error $ "undefined function '" ++ func ++ "'"
