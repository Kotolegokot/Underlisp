module Lib.List (builtin_list,
                 builtin_head,
                 builtin_tail,
                 builtin_null,
                 builtin_append) where

import SExpr
import Exception

builtin_list :: [SExpr] -> IO SExpr
builtin_list = return . list

builtin_head :: [SExpr] -> IO SExpr
builtin_head [SList _ (first:_)] = return first
builtin_head [SList p []]        = report p "empty list"
builtin_head [sexpr]             = report (point sexpr) "list expected"
builtin_head _                   = report_undef "just one argument required"

builtin_tail :: [SExpr] -> IO SExpr
builtin_tail [SList p (_:rest)] = return $ SList p rest
builtin_tail [SList p []]       = report p "empty list"
builtin_tail [sexpr]            = report (point sexpr) "list expected"
builtin_tail _                  = report_undef "just one argument required"

builtin_null :: [SExpr] -> IO SExpr
builtin_null [SList p list]    = return . bool . null $ list
builtin_null [sexpr]           = report (point sexpr) "list expected"
builtin_null _                 = report_undef "just one argument requried"

builtin_append :: [SExpr] -> IO SExpr
builtin_append [list1, list2]
  | not $ is_list list1 = report (point list1) "first argument must be a list"
  | not $ is_list list2 = report (point list2) "second argument must be a list"
  | otherwise           = return $ list (from_list list1 ++ from_list list2)
builtin_append _ = report_undef "two arguments required"
