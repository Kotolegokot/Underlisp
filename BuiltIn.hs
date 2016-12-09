module BuiltIn where

import qualified Data.Map as Map
import Data.Maybe
import Text.Read (readMaybe)
import System.IO (stdout, hFlush)
import SExpr
import Reader

type Eval = Context -> SExpr -> IO SExpr

builtin_let :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_let eval context ((SList pairs):body) = do
    new_context <- handle_pairs pairs context
    eval new_context (SList (SKeyword "seq":body))
        where handle_pairs (x:xs) acc = case x of
                                          (SList [SKeyword var, value]) -> do
                                              exp <- eval acc value
                                              handle_pairs xs (Map.insert var exp acc)
                                          (SList [_, _]) -> error "first item in a let binding pair must be a keyword"
                                          _              -> error "a binding in 'let' must be of the following form: (var value)"

              handle_pairs []     acc = return acc
builtin_let _    _       _               = error "list of bindings expected"

builtin_print :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_print eval context [arg] = eval context arg >>= (putStr . show_sexpr) >> return empty_list
builtin_print _    _       _     = error "'print' requires only one argument"

builtin_print_ln :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_print_ln eval context [arg] = eval context arg >>= (putStrLn . show_sexpr) >> return empty_list
builtin_print_ln _    _       _     = error "'print-ln' requires only one argument"

builtin_flush :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_flush eval context [] = hFlush stdout >> return empty_list
builtin_flush _    _       _  = error "'flush' requires no arguments"

builtin_get_line :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_get_line eval context [] = getLine >>= (return . SString)
builtin_get_line _    _       _  = error "'get-line' requires no arguments"

builtin_type :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_type eval context [arg] = eval context arg >>= (return . SString . show_type)
builtin_type _    _       _     = error "'type' requires only one argument"

builtin_if :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_if eval context [cond_sexpr]                          = builtin_if eval context [cond_sexpr, empty_list, empty_list]
builtin_if eval context [cond_sexpr, true_sexpr]              = builtin_if eval context [cond_sexpr, true_sexpr, empty_list]
builtin_if eval context [cond_sexpr, true_sexpr, false_sexpr] = do
    cond <- eval context cond_sexpr
    if from_bool cond
       then eval context true_sexpr
       else eval context false_sexpr
builtin_if _    _       _                                     = error "'if' requires 1 to 3 arguments"

builtin_unless :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_unless eval context [cond_sexpr]                          = builtin_if eval context [cond_sexpr]
builtin_unless eval context [cond_sexpr, false_sexpr]             = builtin_if eval context [cond_sexpr, empty_list, false_sexpr]
builtin_unless eval context [cond_sexpr, false_sexpr, true_sexpr] = builtin_if eval context [cond_sexpr, true_sexpr, false_sexpr]
builtin_unless _    _       _                                     = error "'unless' requires 1 to 3 arguments"

builtin_eq :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_eq eval context [arg1, arg2] = do
    expr1 <- eval context arg1
    expr2 <- eval context arg2
    return . SBool $ expr1 == expr2
builtin_eq _    _       _            = error "'=' requires two arguments"

builtin_ne :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_ne eval context [arg1, arg2] = do
    expr1 <- eval context arg1
    expr2 <- eval context arg2
    return . SBool $ expr1 /= expr2
builtin_ne _      _       _          = error "'/=' requires two arguments"

builtin_lt :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_lt eval context [arg1, arg2] = do
    expr1 <- eval context arg1
    expr2 <- eval context arg2
    return . SBool $ expr1 < expr2

builtin_lt _    _       _            = error "'<' requires two arguments"

builtin_gt :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_gt eval context [arg1, arg2] = do
    expr1 <- eval context arg1
    expr2 <- eval context arg2
    return . SBool $ expr1 > expr2

builtin_gt _    _       _            = error "'>' requires two arguments"

builtin_le :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_le eval context [arg1, arg2] = do
    expr1 <- eval context arg1
    expr2 <- eval context arg2
    return . SBool $ expr1 <= expr2

builtin_le _    _       _            = error "'<=' requires two arguments"

builtin_ge :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_ge eval context [arg1, arg2] = do
    expr1 <- eval context arg1
    expr2 <- eval context arg2
    return . SBool $ expr1 >= expr2
builtin_ge _    _       _            = error "'>=' requires two arguments"

builtin_not :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_not eval context [arg] = do
    expr <- eval context arg
    return . SBool . not . from_bool $ expr
builtin_not _    _       _     = error "'not' requires only one argument"

builtin_and :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_and eval context (x:xs) = do
            expr <- eval context x
            case from_bool expr of
                   True  -> builtin_and eval context xs
                   False -> return $ SBool False
builtin_and _    _       []     = return $ SBool True

builtin_or :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_or eval context (x:xs) = do
            expr <- eval context x
            case from_bool expr of
              True -> return $ SBool True
              False -> builtin_or eval context xs
builtin_or _    _       []     = return $ SBool False

builtin_impl :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_impl eval context [arg1, arg2] = do
    expr1 <- eval context arg1
    if not $ from_bool expr1
       then return $ SBool True
       else eval context arg2

builtin_impl _    _       _            = error "'->' requires two arguments"

builtin_seq :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_seq eval context [arg]  = eval context arg
builtin_seq eval context (x:xs) = eval context x >> builtin_seq eval context xs
builtin_seq _    _       []     = return empty_list

builtin_sum :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_sum eval context args = do
    (exps, return_type) <- num_args eval context args
    return $ case return_type of
               NTInt   -> SInt . sum . fmap from_int $ exps
               NTFloat -> SFloat . sum . fmap from_number $ exps

builtin_substract :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_substract eval context args@[_, _] = do
    ([x, y], return_type) <- num_args eval context args
    return $ case return_type of
               NTInt   -> SInt   $ from_int x - from_int y
               NTFloat -> SFloat $ from_number x - from_number y
builtin_substract _    _       _           = error "'-' expects requires two arguments"

builtin_product :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_product eval context args = do
    (exps, return_type) <- num_args eval context args
    return $ case return_type of
               NTInt   -> SInt . product . fmap from_int $ exps
               NTFloat -> SFloat . product . fmap from_number $ exps

builtin_divide :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_divide eval context args@[_, _] = do
    ([x, y], return_type) <- num_args eval context args
    return $ case return_type of
               NTInt   -> SInt   $ from_int x `div` from_int y
               NTFloat -> SFloat $ from_number x / from_number y
builtin_divide _    _       _           = error "'-' expects requires two arguments"

data NumType = NTInt | NTFloat
num_args :: Eval -> Context -> [SExpr] -> IO ([SExpr], NumType)
num_args eval context args = helper args [] NTInt
    where helper (x:xs) acc NTInt = do
            exp <- eval context x
            case exp of
              SInt   _ -> helper xs (exp : acc) NTInt
              SFloat _ -> helper xs (exp : acc) NTFloat
              _        -> error "float or int expected"

          helper (x:xs) acc NTFloat = do
              exp <- eval context x
              case exp of
                SInt   _ -> helper xs (exp : acc) NTFloat
                SFloat _ -> helper xs (exp : acc) NTFloat
                _        -> error "float or int expected"

          helper [] acc return_type = return (reverse acc, return_type)

builtin_float :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_float eval context [arg] = do
    expr <- eval context arg
    return $ case expr of
      SFloat float -> SFloat float
      SInt   int   -> SFloat $ fromIntegral int
      _            -> error "float or int expected"
builtin_float _    _       _     = error "'float' requires only one argument"

builtin_concat :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_concat eval context args = helper args ""
    where helper (x:xs) str = do
            expr <- eval context x
            case expr of
              SString string -> helper xs (str ++ string)
              _              -> error "string expected"
          helper [] str     = return $ SString str

builtin_str_to_int :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_str_to_int eval context [arg] = do
    expr <- eval context arg
    case expr of
      SString str -> return . SInt $ case readMaybe str :: Maybe Int of
                                        Just int -> int
                                        Nothing  -> error $ "couldn't convert string to int: '" ++ str ++ "'"
      _           -> error "string expected"

builtin_str_to_int _    _       _     = error "str-to-int requires only one argument"

builtin_str_to_float :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_str_to_float eval context [arg] = do
    expr <- eval context arg
    case expr of
      SString str -> return . SFloat $ case readMaybe str :: Maybe Float of
                                        Just float -> float
                                        Nothing  -> error $ "couldn't convert string to float: '" ++ str ++ "'"
      _           -> error "string expected"
builtin_str_to_float _    _       _     = error "str-to-float requires only one argument"

builtin_list :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_list eval context args = return . SList =<< mapM (eval context) args

builtin_eval :: Eval -> Context -> [SExpr] -> IO SExpr
builtin_eval eval context [SString expr] = (eval Map.empty . Reader.read $ expr) >> return empty_list
builtin_eval _    _       [_]            = error "string expected"
builtin_eval _    _       _              = error "'eval' requires only one argument"

