module Interpreter (interprete) where

import Text.Read
import Control.Monad
import Data.Tree
import qualified Data.Map as Map
import Context
import SemanticAnalyzer

interprete :: Tree Terminal -> IO ()
interprete (Node (TKeyword "program") body) = void $ eval_function (Map.empty) (Node (TKeyword "seq") body)
interprete _ = error "no 'program' at the beginning of the outer list"

eval_function :: Context -> Tree Terminal -> IO Terminal
eval_function context (Node (TKeyword kword) args)
  | kword `Map.member` context = return $ context Map.! kword
  | otherwise                  = call_function context kword args
eval_function context (Node first args) 
  | null args = return first
  | otherwise = error $ "too many arguments for '" ++ printTerminal first ++ "'"

call_function :: Context -> String -> Forest Terminal -> IO Terminal

call_function context "let" args
  | length args <= 1 = error "'let' requires more than one argument"
  | otherwise        = do
      additional_context <- handleBindings (init args) Map.empty
      eval_function (context `Map.union` additional_context) (last args)
          where handleBindings (Node a subtree:xs) add_context
                  | length subtree /= 1 = error "a binding in 'let' must be of the following form: (var value)"
                  | not $ isKeyword  a = error "first item in a let binding pair must be a keyword"
                  | otherwise          = do
                      let (TKeyword var, [value]) = (a, subtree)
                      exp <- eval_function (context `Map.union` add_context) value
                      handleBindings xs (Map.insert var exp add_context)

                handleBindings [] add_context = return add_context

call_function context "print" args
  | length args /= 1 = error "'print' requires only one argument"
  | otherwise        = eval_function context (head args) >>= (putStr . printTerminal) >> return TNil

call_function context "print-ln" args
  | length args /= 1 = error "'print-ln' requires only one argument"
  | otherwise        = eval_function context (head args) >>= (putStrLn . printTerminal) >> return TNil

call_function context "get-line" args
  | not $ null args  = error "'get-line' requires no arguments"
  | otherwise        = getLine >>= (return . TString)

call_function context "type" args
  | length args /= 1 = error "'type' requires only one argument"
  | otherwise        = eval_function context (head args) >>= (return . TString . printType)

call_function context "if" args
  | length args == 1 = call_function context "if" (args ++ [Node TNil [], Node TNil []]) 
  | length args == 2 = call_function context "if" (args ++ [Node TNil []])
  | length args == 3 = handle_if args
  | otherwise = error "'if' requires 1 to 3 arguments"
  where handle_if [arg1, arg2, arg3] = do
          cond <- eval_function context arg1
          if terminalToBool cond
             then eval_function context arg2
             else eval_function context arg3
             
call_function context "unless" args
  | length args == 1 = call_function context "unless" (args ++ [Node TNil [], Node TNil []]) 
  | length args == 2 = call_function context "unless" (args ++ [Node TNil []])
  | length args == 3 = handle_if args
  | otherwise = error "'unless' requires 1 to 3 arguments"
  where handle_if [arg1, arg2, arg3] = do
          cond <- eval_function context arg1
          if terminalToBool cond
             then eval_function context arg3
             else eval_function context arg2

call_function context "=" args
    | length args /= 2 = error "'=' requires two arguments"
    | otherwise = handle_eq args
    where handle_eq [arg1, arg2] = do
              exp1 <- eval_function context arg1
              exp2 <- eval_function context arg2
              return . boolToTerminal $ exp1 == exp2

call_function context "/=" args
    | length args /= 2 = error "'/=' requires two arguments"
    | otherwise = handle_eq args
    where handle_eq [arg1, arg2] = do
              exp1 <- eval_function context arg1
              exp2 <- eval_function context arg2
              return . boolToTerminal $ exp1 /= exp2              

call_function context ">" args
    | length args /= 2 = error "'>' requires two argumens"
    | otherwise = handle_gt args
    where handle_gt [arg1, arg2] = do
              exp1 <- eval_function context arg1
              exp2 <- eval_function context arg2
              return . boolToTerminal $ exp1 > exp2

call_function context "<" args
    | length args /= 2 = error "'<' requires two argumens"
    | otherwise = handle_gt args
    where handle_gt [arg1, arg2] = do
              exp1 <- eval_function context arg1
              exp2 <- eval_function context arg2
              return . boolToTerminal $ exp1 < exp2
              
call_function context ">=" args
    | length args /= 2 = error "'>=' requires two argumens"
    | otherwise = handle_gt args
    where handle_gt [arg1, arg2] = do
              exp1 <- eval_function context arg1
              exp2 <- eval_function context arg2
              return . boolToTerminal $ exp1 >= exp2
              
call_function context "<=" args
    | length args /= 2 = error "'<=' requires two argumens"
    | otherwise = handle_gt args
    where handle_gt [arg1, arg2] = do
              exp1 <- eval_function context arg1
              exp2 <- eval_function context arg2
              return . boolToTerminal $ exp1 <= exp2

call_function context "not" args
    | length args /= 1 = error "'not' requires only one argument"
    | otherwise = do
              exp <- eval_function context $ head args
              return . boolToTerminal . not . terminalToBool $ exp

call_function context "&" args
    | length args <= 2 = error "'&' requires two or more arguments"
    | otherwise = handle_and args
    where handle_and [] = return TT 
          handle_and (x:xs) = do
                exp <- eval_function context x
                case exp of
                  TNil -> return TNil
                  TT -> handle_and xs
                  _ -> error "T or Nil expected"
                  
call_function context "|" args
    | length args <= 2 = error "'|' requires two or more arguments"
    | otherwise = handle_and args
    where handle_and [] = return TNil 
          handle_and (x:xs) = do
                exp <- eval_function context x
                case exp of
                  TT -> return TT
                  TNil -> handle_and xs
                  _ -> error "T or Nil expected"

call_function context "->" args
    | length args /= 2 = error "'->' requires two arguments"
    | otherwise = handle_impl args
    where handle_impl [arg1, arg2] = do
            exp1 <- eval_function context arg1
            exp2 <- eval_function context arg2
            case exp1 of
              TNil -> return TT
              TT   -> case exp2 of
                TNil -> return TNil
                TT   -> return TT
                _    -> error "expected T or Nil"
              _    -> error "expected T or Nil"
                
call_function context "seq" args = handle_seq args
    where handle_seq [x]    = eval_function context x
          handle_seq (x:xs) = eval_function context x >> handle_seq xs
          handle_seq []     = return TNil

call_function context "+" args = do
    (exps, return_type) <- num_args context args

    return $ case return_type of
               ARInt   -> TInt . sum . fmap fromInt $ exps
               ARFloat -> TFloat . sum . fmap fromNumber $ exps

call_function context "-" args
  | length args /= 2 = error "'-' requires two arguments"
  | otherwise        = do
      ([x, y], return_type) <- num_args context args
      
      return $ case return_type of
                 ARInt   -> TInt   $ fromInt x - fromInt y
                 ARFloat -> TFloat $ fromNumber x - fromNumber y

call_function context "*" args = do
    (exps, return_type) <- num_args context args

    return $ case return_type of
               ARInt   -> TInt . product . fmap fromInt $ exps
               ARFloat -> TFloat . product . fmap fromNumber $ exps

call_function context "/" args
  | length args /= 2 = error "'/' requires two arguments"
  | otherwise        = do
      ([x, y], return_type) <- num_args context args

      return $ case return_type of
                 ARInt   -> TInt   $ fromInt x `div` fromInt y
                 ARFloat -> TFloat $ fromNumber x / fromNumber y

call_function context "float" args
  | length args /= 1 = error "'float' requires only one argument"
  | otherwise        = do
      exp <- eval_function context $ head args

      return . TFloat . fromNumber $ exp

call_function context "concat" args = handle_concat args ""
    where handle_concat (x:xs) string = do
            exp <- eval_function context x

            case exp of
              TString str -> handle_concat xs (string ++ str)
              _           -> error "string expected"

          handle_concat [] string = return . TString $ string

call_function context "str-to-int" args
  | length args /= 1 = error "'str-to-int' requires only one argument"
  | otherwise        = do
      exp <- eval_function context $ head args

      return $ case exp of
                 TString str -> TInt $ case readMaybe str :: Maybe Int of
                                        Just int -> int
                                        Nothing  -> error $ "couldn't convert string to int: '" ++ str ++ "'"
                 _           -> error "string expected"

call_function context "list" args = handle_list args []
    where handle_list (x:xs) items = do
            exp <- eval_function context x
            handle_list xs (items ++ [exp])

          handle_list [] items = return . TList $ items
              
call_function _ func _ = error $ "undefined function '" ++ func ++ "'"

data ArithmReturn = ARInt | ARFloat
num_args :: Context -> Forest Terminal -> IO ([Terminal], ArithmReturn)
num_args context args = helper args [] ARInt
    where helper (x:xs) exps ARInt = do
            exp <- eval_function context x
        
            case exp of
              TInt   _ -> helper xs (exps ++ [exp]) ARInt
              TFloat _ -> helper xs (exps ++ [exp]) ARFloat
              _        -> error "float or int expected"

          helper (x:xs) exps ARFloat = do
            exp <- eval_function context x

            case exp of
              TInt   _ -> helper xs (exps ++ [exp]) ARFloat
              TFloat _ -> helper xs (exps ++ [exp]) ARFloat
              _        -> error "float or int expected"

          helper [] exps return_type = return (exps, return_type)
