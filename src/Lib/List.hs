module Lib.List (builtin_list,
                 builtin_head,
                 builtin_tail,
                 builtin_null,
                 builtin_append) where

import SExpr

builtin_list :: [SExpr] -> IO SExpr
builtin_list = return . SList

builtin_head :: [SExpr] -> IO SExpr
builtin_head [SList (first:_)] = return first
builtin_head [SList []]        = error "head: empty list"
builtin_head [_]               = error "head: list expected"
builtin_head _                 = error "head: just one argument required"

builtin_tail :: [SExpr] -> IO SExpr
builtin_tail [SList (_:rest)] = return . SList $ rest
builtin_tail [SList []]       = error "tail: empty list"
builtin_tail [_]              = error "tail: list expected"
builtin_tail _                = error "tail: just one argument required"

builtin_null :: [SExpr] -> IO SExpr
builtin_null [SList list] = return . bool . null $ list
builtin_null [_]           = error "null: list expected"
builtin_null _             = error "null: just one argument requried"

builtin_append :: [SExpr] -> IO SExpr
builtin_append [list1, list2]
  | not $ is_list list1 = error "append: first argument must be a list"
  | not $ is_list list2 = error "append: second argument must be a list"
  | otherwise           = return $ SList (from_list list1 ++ from_list list2)
builtin_append _ = error "append: two arguments required"
