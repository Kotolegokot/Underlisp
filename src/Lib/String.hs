module Lib.String (builtin_concat,
                   builtin_str_to_int,
                   builtin_str_to_float,
                   builtin_str_length) where

import Text.Read (readMaybe)
import SExpr
import Expr

builtin_concat :: [SExpr] -> IO SExpr
builtin_concat sexprs = return . string . concat . fmap from_string $ sexprs

builtin_str_to_int :: [SExpr] -> IO SExpr
builtin_str_to_int [SAtom (AString str)] = case readMaybe str :: Maybe Int of
                                     Just i  -> return $ int i
                                     Nothing -> error $ "str-to-int: failed to convert string to int: '" ++ str ++ "'"
builtin_str_to_int [_]           = error "str-to-int: string expected"
builtin_str_to_int _             = error "str-to-int: just one argument required"

builtin_str_to_float :: [SExpr] -> IO SExpr
builtin_str_to_float [SAtom (AString str)] = case readMaybe str :: Maybe Float of
                                       Just f  -> return $ float f
                                       Nothing -> error $ "str-to-float: failed to convert string to float: '" ++ str ++ "'"
builtin_str_to_float [_]           = error "str-to-float: string expected"
builtin_str_to_float _             = error "str-to-float: just one argument required"

builtin_str_length :: [SExpr] -> IO SExpr
builtin_str_length [SAtom (AString str)] = return . int . length $ str
builtin_str_length [_]           = error "str-length: string expected"
builtin_str_length _             = error "str-length: just one argument required"
