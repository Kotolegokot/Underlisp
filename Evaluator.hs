module Evaluator (evaluate) where

import System.IO
import Text.Read
import qualified Data.Map as Map

import qualified Reader
import SExpr

start_context :: Context
start_context = Map.singleton "f" (SFunc (UserDefined 1 (FRef 1)))

evaluate :: SExpr -> IO ()
evaluate (SList (SKeyword "program":body)) = mapM_ (eval_sexpr start_context) body
evaluate _                                 = error "a program must start with calling 'program'"

eval_sexpr ::Context -> SExpr -> IO SExpr
eval_sexpr context (SList (first:body)) = do
    expr <- eval_sexpr context first
    return $ case expr of
               SFunc (UserDefined count_args fexpr) -> apply (from_func expr) body
               _                                    -> error $ "can't execute sexpr: '" ++ show_sexpr expr ++ "'"
eval_sexpr context (SList [])           = error "can't execute empty list"
eval_sexpr context (SKeyword str) 
  | str `Map.member` context = return $ context Map.! str
  | otherwise                = error $ "undefined identificator '" ++ str ++ "'"
eval_sexpr context sexpr                = return sexpr

call_function :: String -> Context -> [SExpr] -> IO SExpr
call_function fname = case fname of
                        _              -> error $ "undefined function: '" ++ fname ++ "'"

builtin_let :: Context -> [SExpr] -> IO SExpr
builtin_let context ((SList pairs):body) = do
    new_context <- handle_pairs pairs context
    eval_sexpr new_context (SList (SKeyword "seq":body))
        where handle_pairs (x:xs) acc = case x of
                                          (SList [SKeyword var, value]) -> do
                                              exp <- eval_sexpr acc value
                                              handle_pairs xs (Map.insert var exp acc)
                                          (SList [_, _]) -> error "first item in a let binding pair must be a keyword"
                                          _              -> error "a binding in 'let' must be of the following form: (var value)"
              
              handle_pairs []     acc = return acc
              
builtin_let _       _                    = error "list of bindings expected"

builtin_print :: Context -> [SExpr] -> IO SExpr
builtin_print context [arg] = eval_sexpr context arg >>= (putStr . show_sexpr) >> return empty_list
builtin_print _       _     = error "'print' requires only one argument"

builtin_print_ln :: Context -> [SExpr] -> IO SExpr
builtin_print_ln context [arg] = eval_sexpr context arg >>= (putStrLn . show_sexpr) >> return empty_list
builtin_print_ln _       _     = error "'print-ln' requires only one argument"

builtin_flush :: Context -> [SExpr] -> IO SExpr
builtin_flush context [] = hFlush stdout >> return empty_list
builtin_flush _       _  = error "'flush' requires no arguments"

builtin_get_line :: Context -> [SExpr] -> IO SExpr
builtin_get_line context [] = getLine >>= (return . SString)
builtin_get_line _       _  = error "'get-line' requires no arguments"

builtin_type :: Context -> [SExpr] -> IO SExpr
builtin_type context [arg] = eval_sexpr context arg >>= (return . SString . show_type)
builtin_type _       _     = error "'type' requires only one argument"

builtin_if :: Context -> [SExpr] -> IO SExpr
builtin_if context [cond_sexpr]                          = builtin_if context [cond_sexpr, empty_list, empty_list]
builtin_if context [cond_sexpr, true_sexpr]              = builtin_if context [cond_sexpr, true_sexpr, empty_list]
builtin_if context [cond_sexpr, true_sexpr, false_sexpr] = do
    cond <- eval_sexpr context cond_sexpr
    if from_bool cond
       then eval_sexpr context true_sexpr
       else eval_sexpr context false_sexpr
builtin_if _       _                                     = error "'if' requires 1 to 3 arguments"

builtin_unless :: Context -> [SExpr] -> IO SExpr
builtin_unless context [cond_sexpr]                          = builtin_if context [cond_sexpr]
builtin_unless context [cond_sexpr, false_sexpr]             = builtin_if context [cond_sexpr, empty_list, false_sexpr]
builtin_unless context [cond_sexpr, false_sexpr, true_sexpr] = builtin_if context [cond_sexpr, true_sexpr, false_sexpr]
builtin_unless _       _                                     = error "'unless' requires 1 to 3 arguments"

builtin_eq :: Context -> [SExpr] -> IO SExpr
builtin_eq context [arg1, arg2] = do
    expr1 <- eval_sexpr context arg1
    expr2 <- eval_sexpr context arg2
    return . SBool $ expr1 == expr2
builtin_eq _       _            = error "'=' requires two arguments"

builtin_ne :: Context -> [SExpr] -> IO SExpr
builtin_ne context [arg1, arg2] = do
    expr1 <- eval_sexpr context arg1
    expr2 <- eval_sexpr context arg2
    return . SBool $ expr1 /= expr2
builtin_ne _       _            = error "'/=' requires two arguments"

builtin_lt :: Context -> [SExpr] -> IO SExpr
builtin_lt context [arg1, arg2] = do
    expr1 <- eval_sexpr context arg1
    expr2 <- eval_sexpr context arg2
    return . SBool $ expr1 < expr2
builtin_lt _       _            = error "'<' requires two arguments"

builtin_gt :: Context -> [SExpr] -> IO SExpr
builtin_gt context [arg1, arg2] = do
    expr1 <- eval_sexpr context arg1
    expr2 <- eval_sexpr context arg2
    return . SBool $ expr1 > expr2
builtin_gt _       _            = error "'>' requires two arguments"

builtin_le :: Context -> [SExpr] -> IO SExpr
builtin_le context [arg1, arg2] = do
    expr1 <- eval_sexpr context arg1
    expr2 <- eval_sexpr context arg2
    return . SBool $ expr1 <= expr2
builtin_le _       _            = error "'<=' requires two arguments"

builtin_ge :: Context -> [SExpr] -> IO SExpr
builtin_ge context [arg1, arg2] = do
    expr1 <- eval_sexpr context arg1
    expr2 <- eval_sexpr context arg2
    return . SBool $ expr1 >= expr2
builtin_ge _       _            = error "'>=' requires two arguments"

builtin_not :: Context -> [SExpr] -> IO SExpr
builtin_not context [arg] = do
    expr <- eval_sexpr context arg
    return . SBool . not . from_bool $ expr
builtin_not _       _     = error "'not' requires only one argument"

builtin_and :: Context -> [SExpr] -> IO SExpr
builtin_and context (x:xs) = do
            expr <- eval_sexpr context x
            case from_bool expr of
                   True  -> builtin_and context xs
                   False -> return $ SBool False
builtin_and _       []     = return $ SBool True

builtin_or :: Context -> [SExpr] -> IO SExpr
builtin_or context (x:xs) = do
            expr <- eval_sexpr context x
            case from_bool expr of
              True -> return $ SBool True
              False -> builtin_or context xs
builtin_or _       []     = return $ SBool False

builtin_impl :: Context -> [SExpr] -> IO SExpr
builtin_impl context [arg1, arg2] = do
    expr1 <- eval_sexpr context arg1
    if not $ from_bool expr1
       then return $ SBool True
       else eval_sexpr context arg2
builtin_impl _       _            = error "'->' requires two arguments"

builtin_seq :: Context -> [SExpr] -> IO SExpr
builtin_seq context [arg]  = eval_sexpr context arg
builtin_seq context (x:xs) = eval_sexpr context x >> builtin_seq context xs
builtin_seq context []     = return empty_list

builtin_sum :: Context -> [SExpr] -> IO SExpr
builtin_sum context args = do
    (exps, return_type) <- num_args context args
    return $ case return_type of
               NTInt   -> SInt . sum . fmap from_int $ exps
               NTFloat -> SFloat . sum . fmap from_number $ exps

builtin_substract :: Context -> [SExpr] -> IO SExpr
builtin_substract context args@[_, _] = do
    ([x, y], return_type) <- num_args context args
    return $ case return_type of
               NTInt   -> SInt   $ from_int x - from_int y
               NTFloat -> SFloat $ from_number x - from_number y
builtin_substract _       _            = error "'-' expects requires two arguments"

builtin_product :: Context -> [SExpr] -> IO SExpr
builtin_product context args = do
    (exps, return_type) <- num_args context args
    return $ case return_type of
               NTInt   -> SInt . product . fmap from_int $ exps
               NTFloat -> SFloat . product . fmap from_number $ exps

builtin_divide :: Context -> [SExpr] -> IO SExpr
builtin_divide context args@[_, _] = do
    ([x, y], return_type) <- num_args context args
    return $ case return_type of
               NTInt   -> SInt   $ from_int x `div` from_int y
               NTFloat -> SFloat $ from_number x / from_number y

data NumType = NTInt | NTFloat
num_args :: Context -> [SExpr] -> IO ([SExpr], NumType)
num_args context args = helper args [] NTInt
    where helper (x:xs) acc NTInt = do
            exp <- eval_sexpr context x
            case exp of
              SInt   _ -> helper xs (exp : acc) NTInt
              SFloat _ -> helper xs (exp : acc) NTFloat
              _        -> error "float or int expected"

          helper (x:xs) acc NTFloat = do
              exp <- eval_sexpr context x
              case exp of
                SInt   _ -> helper xs (exp : acc) NTFloat
                SFloat _ -> helper xs (exp : acc) NTFloat
                _        -> error "float or int expected"

          helper [] acc return_type = return (reverse acc, return_type)

builtin_float :: Context -> [SExpr] -> IO SExpr
builtin_float context [arg] = do
    expr <- eval_sexpr context arg
    return $ case expr of
      SFloat float -> SFloat float
      SInt   int   -> SFloat $ fromIntegral int
      _            -> error "float or int expected"
builtin_float _       _     = error "'float' requires only one argument"

builtin_concat :: Context -> [SExpr] -> IO SExpr
builtin_concat context args = helper args ""
    where helper (x:xs) str = do
            expr <- eval_sexpr context x
            case expr of
              SString string -> helper xs (str ++ string)
              _              -> error "string expected"
          helper [] str     = return $ SString str

builtin_str_to_int :: Context -> [SExpr] -> IO SExpr
builtin_str_to_int context [arg] = do
    expr <- eval_sexpr context arg
    case expr of
      SString str -> return . SInt $ case readMaybe str :: Maybe Int of
                                        Just int -> int
                                        Nothing  -> error $ "couldn't convert string to int: '" ++ str ++ "'"
      _           -> error "string expected"
builtin_str_to_int _ _           = error "str-to-int requires only one argument"

builtin_str_to_float :: Context -> [SExpr] -> IO SExpr
builtin_str_to_float context [arg] = do
    expr <- eval_sexpr context arg
    case expr of
      SString str -> return . SFloat $ case readMaybe str :: Maybe Float of
                                        Just float -> float
                                        Nothing  -> error $ "couldn't convert string to float: '" ++ str ++ "'"
      _           -> error "string expected"
builtin_str_to_float _       _     = error "str-to-float requires only one argument"

builtin_list :: Context -> [SExpr] -> IO SExpr
builtin_list context args = return . SList =<< mapM (eval_sexpr context) args

builtin_eval :: Context -> [SExpr] -> IO SExpr
builtin_eval context [SString expr] = (eval_sexpr Map.empty . Reader.read $ expr) >> return empty_list
builtin_eval _       [_]            = error "string expected"
builtin_eval _       _              = error "'eval' requires only one argument"

