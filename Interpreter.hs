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
  | isKeyword head = call_function functions (fromKeyword head) args
  | otherwise      = if null args then return head else error $ "too many arguments for '" ++ printTerminal head ++ "'"

call_function :: Map.Map String Function -> String -> Forest Terminal -> IO Terminal

call_function functions "print" args
  | length args /= 1 = error "'print' requires only one argument"
  | otherwise        = eval_function functions (head args) >>= (putStr . printTerminal) >> return TNil

call_function functions "print-ln" args
  | length args /= 1 = error "'print-ln' requires only one argument"
  | otherwise        = eval_function functions (head args) >>= (putStrLn . printTerminal) >> return TNil

call_function functions "type" args
  | length args /= 1 = error "'type' requires only one argument"
  | otherwise        = eval_function functions (head args) >>= (return . TString . printType)

call_function functions "if" args
  | length args == 1 = call_function functions "if" (args ++ [Node TNil [], Node TNil []]) 
  | length args == 2 = call_function functions "if" (args ++ [Node TNil []])
  | length args == 3 = handle_if args
  | otherwise = error "'if' requires 1 to 3 arguments"
  where handle_if [arg1, arg2, arg3] = do
          cond <- eval_function functions arg1
          if terminalToBool cond
             then eval_function functions arg2
             else eval_function functions arg3
             
call_function functions "unless" args
  | length args == 1 = call_function functions "unless" (args ++ [Node TNil [], Node TNil []]) 
  | length args == 2 = call_function functions "unless" (args ++ [Node TNil []])
  | length args == 3 = handle_if args
  | otherwise = error "'unless' requires 1 to 3 arguments"
  where handle_if [arg1, arg2, arg3] = do
          cond <- eval_function functions arg1
          if terminalToBool cond
             then eval_function functions arg3
             else eval_function functions arg2

call_function functions "=" args
    | length args /= 2 = error "'=' requires two arguments"
    | otherwise = handle_eq args
    where handle_eq [arg1, arg2] = do
              exp1 <- eval_function functions arg1
              exp2 <- eval_function functions arg2
              return . boolToTerminal $ exp1 == exp2

call_function functions "/=" args
    | length args /= 2 = error "'/=' requires two arguments"
    | otherwise = handle_eq args
    where handle_eq [arg1, arg2] = do
              exp1 <- eval_function functions arg1
              exp2 <- eval_function functions arg2
              return . boolToTerminal $ exp1 /= exp2              

call_function functions "seq" args = handle_seq args
    where handle_seq [x]    = eval_function functions x
          handle_seq (x:xs) = eval_function functions x >> handle_seq xs
          handle_seq []     = return TNil

call_function functions "+" args = do
    (exps, return_type) <- num_args functions args

    return $ case return_type of
               ARInt   -> TInt . sum . fmap fromInt $ exps
               ARFloat -> TFloat . sum . fmap fromNumber $ exps

call_function functions "-" args
  | length args /= 2 = error "'-' requires two arguments"
  | otherwise        = do
      ([x, y], return_type) <- num_args functions args
      
      return $ case return_type of
                 ARInt   -> TInt   $ fromInt x - fromInt y
                 ARFloat -> TFloat $ fromNumber x - fromNumber y

call_function functions "*" args = do
    (exps, return_type) <- num_args functions args

    return $ case return_type of
               ARInt   -> TInt . product . fmap fromInt $ exps
               ARFloat -> TFloat . product . fmap fromNumber $ exps

call_function functions "/" args
  | length args /= 2 = error "'/' requires two arguments"
  | otherwise        = do
      ([x, y], return_type) <- num_args functions args

      return $ case return_type of
                 ARInt   -> TInt   $ fromInt x `div` fromInt y
                 ARFloat -> TFloat $ fromNumber x / fromNumber y
              
call_function _ func _ = error $ "undefined function '" ++ func ++ "'"

data ArithmReturn = ARInt | ARFloat
num_args :: Map.Map String Function -> Forest Terminal -> IO ([Terminal], ArithmReturn)
num_args functions args = helper args [] ARInt
    where helper (x:xs) exps ARInt = do
            exp <- eval_function functions x
        
            case exp of
              TInt   _ -> helper xs (exps ++ [exp]) ARInt
              TFloat _ -> helper xs (exps ++ [exp]) ARFloat
              _        -> error "float or int expected"

          helper (x:xs) exps ARFloat = do
            exp <- eval_function functions x

            case exp of
              TInt   _ -> helper xs (exps ++ [exp]) ARFloat
              TFloat _ -> helper xs (exps ++ [exp]) ARFloat
              _        -> error "float or int expected"

          helper [] exps return_type = return (exps, return_type)

{--
-- built-in functions
call_function :: Map.Map String Function -> String -> [Terminal] -> IO Terminal
call_function _ "<"        args = lt_ args
call_function _ ">"        args = gt_ args
call_function _ "<="       args = le_ args
call_function _ ">="       args = ge_ args
call_function _ "&"        args = and_ args
call_function _ "|"        args = or_ args
call_function _ "->"       args = impl_ args
call_function _ "not"      args = not_ args
call_function _ "float"    args = float_ args
--}
