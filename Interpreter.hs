module Interpreter (interprete) where

import Text.Read
import Control.Monad
import Data.Tree
import System.IO
import qualified Data.Map as Map

import SExpr
import qualified Reader
import qualified Evaluator

-- | a lisp interpretator is just a reader and evaluator joined together
interprete :: String -> IO ()
interprete = Evaluator.evaluate . Reader.read

{--

call_function context "let" args
  | length args <= 1 = error "'let' requires more than one argument"
  | otherwise        = do
      additional_context <- handleBindings (init args) Map.empty
      eval_function (context `Map.union` additional_context) (last args)
          where handleBindings (Node a subtree:xs) add_context
                  | length subtree /= 1 = error "a binding in 'let' must be of the following form: (var value)"
                  | not $ isKeyword a   = error "first item in a let binding pair must be a keyword"
                  | otherwise           = do
                      let (TKeyword var, [value]) = (a, subtree)
                      exp <- eval_function (context `Map.union` add_context) value
                      handleBindings xs (Map.insert var exp add_context)

                handleBindings [] add_context = return add_context
                
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
num_args :: Context -> [SExpr] -> IO ([Atom], ArithmReturn)
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
          --}
