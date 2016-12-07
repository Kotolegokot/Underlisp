module Interpreter (interprete, evaluate) where

import Control.Monad
import Data.Tree
import qualified Data.Map as Map
import Program
import SemanticAnalyzer

interprete :: Tree Terminal -> Program
interprete (Node (TKeyword "program") body) = Program Map.empty $ head body
interprete _ = error "no 'program' at the beginning of the outer list"

evaluate :: Program -> IO ()
evaluate program@(Program functions body) = void $ eval_function functions body

eval_function :: Map.Map String Function -> Tree Terminal -> IO Terminal
eval_function functions (Node head args)
  | isKeyword head = call_function functions (fromKeyword head) =<< mapM (eval_function functions) args
  | otherwise      = if null args then return head else error "too many arguments"

call_function :: Map.Map String Function -> String -> [Terminal] -> IO Terminal
call_function functions "print" args
  | length args /= 1 = error "'print' expected only one argument"
  | otherwise        = print (head args) >> return TNil
