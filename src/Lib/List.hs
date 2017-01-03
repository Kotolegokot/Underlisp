module Lib.List (builtinFunctions
                ,specialOperators) where

import Base
import Exception

biList :: [SExpr] -> IO SExpr
biList = return . list

biHead :: [SExpr] -> IO SExpr
biHead [SList _ (first:_)] = return first
biHead [SList p []]        = report p "empty list"
biHead [sexpr]             = report (point sexpr) "list expected"
biHead _                   = reportUndef "just one argument required"

biTail :: [SExpr] -> IO SExpr
biTail [SList p (_:rest)] = return $ SList p rest
biTail [SList p []]       = report p "empty list"
biTail [sexpr]            = report (point sexpr) "list expected"
biTail _                  = reportUndef "just one argument required"

biIsEmpty :: [SExpr] -> IO SExpr
biIsEmpty [SList p list]    = return . bool . null $ list
biIsEmpty [sexpr]           = report (point sexpr) "list expected"
biIsEmpty _                 = reportUndef "just one argument requried"

biAppend :: [SExpr] -> IO SExpr
biAppend [list1, list2]
  | not $ isList list1 = report (point list1) "first argument must be a list"
  | not $ isList list2 = report (point list2) "second argument must be a list"
  | otherwise          = return $ list (fromList list1 ++ fromList list2)
biAppend _ = reportUndef "two arguments required"

builtinFunctions = [("list",   Nothing,          biList)
                   ,("head",   Just (1 :: Int),  biHead)
                   ,("tail",   Just 1,           biTail)
                   ,("empty?", Just 1,           biIsEmpty)
                   ,("append", Just 2,           biAppend)]

specialOperators = []
