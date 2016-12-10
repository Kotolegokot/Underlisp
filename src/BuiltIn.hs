module BuiltIn where

import qualified Data.Map as Map
import Data.Maybe
import Data.List (elemIndex)
import Text.Read (readMaybe)
import System.IO (stdout, hFlush)
import Control.Monad (foldM)
import SExpr
import Reader

type Eval = Context -> SExpr -> IO (SExpr, Context)

eval_list :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
eval_list eval context sexprs = do
    (expr, _) <- foldM (\(_, prev_context) sexpr -> eval prev_context sexpr) (empty_list, context) sexprs
    return (expr, context)

builtin_let :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_let eval context ((SList pairs):body) = do
    new_context <- handle_pairs pairs context
    eval new_context (SList (SKeyword "seq":body))
        where handle_pairs (x:xs) acc = case x of
                                          (SList [SKeyword var, value]) -> do
                                              (expr, _) <- eval acc value
                                              handle_pairs xs (Map.insert var expr acc)
                                          (SList [_, _]) -> error "first item in a let binding pair must be a keyword"
                                          _              -> error "a binding in 'let' must be of the following form: (var value)"

              handle_pairs []     acc = return acc
builtin_let _    _       _               = error "list of bindings expected"

builtin_lambda :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_lambda eval context (SList args:body)
  | not $ all is_keyword args = error "every argument in 'define' must be a keyword"
  | otherwise                 = return (SFunc (UserDefined (length args) (FList $ FKeyword "seq" : fmap handle_sexpr body)), context)
    where handle_sexpr (SList list)         = FList . fmap handle_sexpr $ list
          handle_sexpr kword@(SKeyword str) = case elemIndex kword args of
                                                Just index -> FRef index
                                                Nothing    -> FKeyword str
          handle_sexpr sexpr                = sexpr2fexpr sexpr
builtin_lambda _    _       (_:_) = error "first argument of 'define' must be an argument list"
builtin_lambda _    _       _     = error "'define' requires at least one argument"

builtin_define :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_define eval context (first:second:body)
  | not $ is_keyword first                    = error "first argument of 'define' must be a keyword"
  | not $ is_list second                      = error "second argument of 'define' must be a list"
  | not . all is_keyword . from_list $ second = error "arguments in 'define' must be keywords"
  | otherwise                                 = return (empty_list, Map.insert name (SFunc func) context)
      where name = from_keyword first
            args = from_list second
            func = UserDefined (length args) (FList $ FKeyword "seq" : fmap handle_sexpr body)
            handle_sexpr (SList list)         = FList . fmap handle_sexpr $ list
            handle_sexpr kword@(SKeyword str) = case elemIndex kword args of
                                                  Just index -> FRef index
                                                  Nothing    -> FKeyword str
            handle_sexpr sexpr                = sexpr2fexpr sexpr
builtin_define _    _       _                = error "'define' requires at least two arguments"

builtin_print :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_print eval context [arg] = do
    (expr, _) <- eval context arg
    putStr $ show_sexpr expr
    return (empty_list, context)
builtin_print _    _       _     = error "'print' requires only one argument"

builtin_print_ln :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_print_ln eval context [arg] = do
    (expr, _) <- eval context arg 
    putStrLn $ show_sexpr expr
    return (empty_list, context)
builtin_print_ln _    _       _     = error "'print-ln' requires only one argument"

builtin_flush :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_flush eval context [] = hFlush stdout >> return (empty_list, context)
builtin_flush _    _       _  = error "'flush' requires no arguments"

builtin_get_line :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_get_line eval context [] = do
    line <- getLine
    return (SString line, context)
builtin_get_line _    _       _  = error "'get-line' requires no arguments"

builtin_type :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_type eval context [arg] = do
    (expr, _) <- eval context arg
    return (SString $ show_type expr, context)
builtin_type _    _       _     = error "'type' requires only one argument"

builtin_if :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_if eval context [cond_sexpr]                          = builtin_if eval context [cond_sexpr, empty_list, empty_list]
builtin_if eval context [cond_sexpr, true_sexpr]              = builtin_if eval context [cond_sexpr, true_sexpr, empty_list]
builtin_if eval context [cond_sexpr, true_sexpr, false_sexpr] = do
    (cond, _) <- eval context cond_sexpr
    if from_bool cond
       then eval context true_sexpr
       else eval context false_sexpr
builtin_if _    _       _                                     = error "'if' requires 1 to 3 arguments"

builtin_unless :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_unless eval context [cond_sexpr]                          = builtin_if eval context [cond_sexpr]
builtin_unless eval context [cond_sexpr, false_sexpr]             = builtin_if eval context [cond_sexpr, empty_list, false_sexpr]
builtin_unless eval context [cond_sexpr, false_sexpr, true_sexpr] = builtin_if eval context [cond_sexpr, true_sexpr, false_sexpr]
builtin_unless _    _       _                                     = error "'unless' requires 1 to 3 arguments"

builtin_eq :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_eq eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 == expr2, context)
builtin_eq _    _       _            = error "'=' requires two arguments"

builtin_ne :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_ne eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 /= expr2, context)
builtin_ne _      _       _          = error "'/=' requires two arguments"

builtin_lt :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_lt eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 < expr2, context)
builtin_lt _    _       _            = error "'<' requires two arguments"

builtin_gt :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_gt eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 > expr2, context)


builtin_le :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_le eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 <= expr2, context)
builtin_le _    _       _            = error "'<=' requires two arguments"

builtin_ge :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_ge eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    (expr2, _) <- eval context arg2
    return (SBool $ expr1 >= expr2, context)
builtin_ge _    _       _            = error "'>=' requires two arguments"

builtin_not :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_not eval context [arg] = do
    (expr, _) <- eval context arg
    return (SBool . not . from_bool $ expr, context)
builtin_not _    _       _     = error "'not' requires only one argument"

builtin_and :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_and eval context (x:xs) = do
    (expr, _) <- eval context x
    case from_bool expr of
           True  -> builtin_and eval context xs
           False -> return (SBool False, context)
builtin_and _    context []     = return (SBool True, context)

builtin_or :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_or eval context (x:xs) = do
    (expr, _) <- eval context x
    case from_bool expr of
      True  -> return (SBool True, context)
      False -> builtin_or eval context xs
builtin_or _    context []     = return (SBool False, context)

builtin_impl :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_impl eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    if not $ from_bool expr1
       then return (SBool True, context)
       else eval context arg2
builtin_impl _    _       _            = error "'->' requires two arguments"

builtin_seq :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_seq eval context args = do
    (expr, _) <- eval_list eval context args
    return (expr, context)

builtin_sum :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_sum eval context args = do
    (exps, return_type) <- num_args eval context args
    return (case return_type of
              NTInt   -> SInt . sum . fmap from_int $ exps
              NTFloat -> SFloat . sum . fmap from_number $ exps,
            context)

builtin_substract :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_substract eval context args@[_, _] = do
    ([x, y], return_type) <- num_args eval context args
    return (case return_type of
              NTInt   -> SInt   $ from_int x - from_int y
              NTFloat -> SFloat $ from_number x - from_number y,
            context)
builtin_substract _    _       _           = error "'-' expects requires two arguments"

builtin_product :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_product eval context args = do
    (exps, return_type) <- num_args eval context args
    return (case return_type of
              NTInt   -> SInt . product . fmap from_int $ exps
              NTFloat -> SFloat . product . fmap from_number $ exps,
            context)

builtin_divide :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_divide eval context args@[_, _] = do
    ([x, y], return_type) <- num_args eval context args
    return (case return_type of
              NTInt   -> SInt   $ from_int x `div` from_int y
              NTFloat -> SFloat $ from_number x / from_number y,
            context)
builtin_divide _    _       _           = error "'-' expects requires two arguments"

data NumType = NTInt | NTFloat
num_args :: Eval -> Context -> [SExpr] -> IO ([SExpr], NumType)
num_args eval context args = helper args [] NTInt
    where helper (x:xs) acc NTInt = do
            (expr, _) <- eval context x
            case expr of
              SInt   _ -> helper xs (expr : acc) NTInt
              SFloat _ -> helper xs (expr : acc) NTFloat
              _        -> error "float or int expected"

          helper (x:xs) acc NTFloat = do
              (expr, _) <- eval context x
              case expr of
                SInt   _ -> helper xs (expr : acc) NTFloat
                SFloat _ -> helper xs (expr : acc) NTFloat
                _        -> error "float or int expected"

          helper [] acc return_type = return (reverse acc, return_type)

builtin_float :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_float eval context [arg] = do
    (expr, _) <- eval context arg
    return (case expr of
              SFloat float -> SFloat float
              SInt   int   -> SFloat $ fromIntegral int
              _            -> error "float or int expected",
            context)
builtin_float _    _       _     = error "'float' requires only one argument"

builtin_concat :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_concat eval context args = helper args ""
    where helper (x:xs) str = do
            (expr, _) <- eval context x
            case expr of
              SString string -> helper xs (str ++ string)
              _              -> error "string expected"
          helper [] str     = return (SString str, context)

builtin_str_to_int :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_str_to_int eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      SString str -> return (SInt $ case readMaybe str :: Maybe Int of
                                      Just int -> int
                                      Nothing  -> error $ "couldn't convert string to int: '" ++ str ++ "'",
                             context)
      _           -> error "string expected"

builtin_str_to_int _    _       _     = error "str-to-int requires only one argument"

builtin_str_to_float :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_str_to_float eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      SString str -> return (SFloat $ case readMaybe str :: Maybe Float of
                                        Just float -> float
                                        Nothing  -> error $ "couldn't convert string to float: '" ++ str ++ "'",
                             context)
      _           -> error "string expected"
builtin_str_to_float _    _       _     = error "str-to-float requires only one argument"

builtin_list :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_list eval context args = do
    exprs <- mapM (eval context) args
    return (SList . fmap fst $ exprs, context)

builtin_str_length :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_str_length eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SString string -> return (SInt $ length string, context)
    _     -> error "string expected"

builtin_str_length _    _       _     = error "str-length requires only one argument"

builtin_head :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_head eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SList xs@(_:_) -> return (head xs, context)
    SList []       -> error "head: empty list"
    _              -> error "list expected"

builtin_head _    _       _     = error "'head' requires only one argument"

builtin_tail :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_tail eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SList xs@(_:_)     -> return (SList $ tail xs, context)
    SList []           -> error "tail: empty list"
    _                  -> error "list expected"
builtin_tail _    _       _     = error "'tail' requires only one argument"

builtin_init :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_init eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SList xs@(_:_) -> return (SList $ init xs, context)
    SList []       -> error "init: empty list"
    _              -> error "list expected"
builtin_init _    _       _     = error "'init' requires only one argument"

builtin_last :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_last eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SList xs@(_:_) -> return (last xs, context)
    SList []       -> error "last: empty list"
    _              -> error "list expected"
builtin_last _    _       _     = error "'last' requires only one argument"

builtin_length :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_length eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SList xs -> return (SInt $ length xs, context)
    _        -> error "list expected"
builtin_length _    _       _     = error "length requires only one argument"

builtin_append :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_append eval context args = helper args []
  where helper (x:xs) acc = do
          (expr, _) <- eval context x
          case expr of
            SList list -> helper xs (acc ++ list)
            _          -> error "list expected"
        helper [] acc     = return (SList acc, context)

builtin_nth :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_nth eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    case expr1 of
      SInt n -> if n < 0 then error "nth: expect positive number" else do
          (expr2, _) <- eval context arg2
          case expr2 of
            SList list -> if length list <= n then error "nth: out of bounds" else return (list !! n, context)
            _          -> error "nth: second argument must be a list"
      _      -> error "nth: first argument must be an int" 
builtin_nth _    _       _            = error "'nth' requires two arguments"

builtin_quote :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_quote eval context [arg] = return (arg, context)
builtin_quote _    _       _     = error "'quote' requires only one argument"

builtin_interprete :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_interprete eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      SString str -> (eval context . Reader.read $ str) >> (return (empty_list, context))
      _           -> error "string expected"
builtin_interprete _    _       _              = error "'interprete' requires only one argument"

builtin_eval :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_eval eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      list@(SList _) -> eval context list
      _              -> error "list expected"
builtin_eval _    _       _                = error "'eval' requires only one argument"
