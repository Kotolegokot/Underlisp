module Lib.String (builtin_concat,
                   builtin_str_to_int,
                   builtin_str_to_float,
                   builtin_str_length) where

import Text.Read (readMaybe)
import Expr
import Lib.Internal

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

builtin_str_to_int _    _       _     = error "str-to-int requires just one argument"

builtin_str_to_float :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_str_to_float eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      SString str -> return (SFloat $ case readMaybe str :: Maybe Float of
                                        Just float -> float
                                        Nothing  -> error $ "couldn't convert string to float: '" ++ str ++ "'",
                             context)
      _           -> error "string expected"
builtin_str_to_float _    _       _     = error "str-to-float requires just one argument"

builtin_str_length :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_str_length eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SString string -> return (SInt $ length string, context)
    _     -> error "string expected"

builtin_str_length _    _       _     = error "str-length requires just one argument"

