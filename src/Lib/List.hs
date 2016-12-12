module Lib.List (builtin_list,
                 builtin_head,
                 builtin_tail,
                 builtin_init,
                 builtin_last,
                 builtin_length,
                 builtin_append,
                 builtin_nth) where

import SExpr
import Lib.Internal

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

builtin_init :: [SExpr] -> IO SExpr
builtin_init [SList list]
  | null list = error "init: empty list"
  | otherwise = return . SList . init $ list
builtin_init [_] = error "init: list expected"
builtin_init _   = error "init: just one argument required"

builtin_last :: [SExpr] -> IO SExpr
builtin_last [SList list]
  | null list = error "last: empty list"
  | otherwise = return $ last list
builtin_last [_] = error "last: list expected"
builtin_last _   = error "last: just one argument required"

builtin_length :: [SExpr] -> IO SExpr
builtin_length [SList list] = return . SInt . length $ list
builtin_length [_]          = error "length: list expected"
builtin_length _            = error "length: just one argument required"

builtin_append :: [SExpr] -> IO SExpr
builtin_append [list1, list2]
  | not $ is_list list1 = error "append: first argument must be a list"
  | not $ is_list list2 = error "append: second argument must be a list"
  | otherwise           = return $ SList (from_list list1 ++ from_list list2)
builtin_append _ = error "append: two arguments required"

builtin_nth :: [SExpr] -> IO SExpr
builtin_nth [list, index]
  | not $ is_list list                        = error "nth: first argument must be a list"
  | not $ is_int index                        = error "nth: second argument must be integer"
  | length (from_list list) >= from_int index = error "nth: out of bounds"
  | otherwise                                 = return $ from_list list !! from_int index
builtin_nth _ = error "nth: two arguments requried"
