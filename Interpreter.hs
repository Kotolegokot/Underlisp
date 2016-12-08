module Interpreter (interprete, evaluate) where

import Control.Monad
import Data.Tree
import qualified Data.Map as Map
import Program
import SemanticAnalyzer

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

-- print function
call_function _ "print" args
  | length args /= 1 = error "'print' expected only one argument"
  | otherwise        = (putStr . printTerminal) (head args) >> return TNil

-- type function
call_function _ "type" args
  | length args /= 1 = error "'type' expected only one argument"
  | otherwise        = return . TString . printType $ head args

-- plus function
call_function _ "+" args = return $ case check_num_args args of
                                      ARFloat -> TFloat . sum $ fmap fromNumber args
                                      ARInt   -> TInt . sum $ fmap fromInt args

-- minus function
call_function _ "-" args@(x:y:[]) = return $ case check_num_args args of
                                               ARFloat -> TFloat $ fromNumber x - fromNumber y
                                               ARInt   -> TInt $ fromInt x - fromInt y
call_function _ "-" _ = error "'-' expected two arguments"

-- product function
call_function _ "*" args = return $ case check_num_args args of
                                      ARFloat -> TFloat . product $ fmap fromNumber args
                                      ARInt   -> TInt . product $ fmap fromInt args

-- divide function
call_function _ "/" args@(x:y:[]) = return $ case check_num_args args of
                                              ARFloat -> TFloat $ fromNumber x / fromNumber y
                                              ARInt   -> TInt $ fromInt x `div` fromInt y
call_function _ "/" _ = error "'/' expected two arguments"


call_function _ _ _ = error "undefined function"

data ArithmReturn = ARInt | ARFloat
check_num_args :: [Terminal] -> ArithmReturn
check_num_args args = helper args ARInt
    where helper (x:xs) ARInt = case x of
                                  TInt _   -> helper xs ARInt
                                  TFloat _ -> helper xs ARFloat
                                  _        -> error "float or int expected"

          helper (x:xs) ARFloat = case x of
                                   TInt _   -> helper xs ARFloat
                                   TFloat _ -> helper xs ARFloat
                                   _        -> error "float or int expected"

          helper [] return_type = return_type
