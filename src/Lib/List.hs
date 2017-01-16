module Lib.List (builtinFunctions
                ,specialOperators) where

import Base
import Fail

biList :: [SExpr] -> Eval SExpr
biList = return . list

biHead :: [SExpr] -> Eval SExpr
biHead [SList _ (first:_)] = return first
biHead [SList p []]        = report p "empty list"
biHead [sexpr]             = report (point sexpr) "list expected"
biHead _                   = reportUndef "just one argument required"

biTail :: [SExpr] -> Eval SExpr
biTail [SList p (_:rest)] = return $ SList p rest
biTail [SList p []]       = report p "empty list"
biTail [sexpr]            = report (point sexpr) "list expected"
biTail _                  = reportUndef "just one argument required"

biAppend :: [SExpr] -> Eval SExpr
biAppend [list1, list2]
  | not $ isList list1 = report (point list1) "first argument must be a list"
  | not $ isList list2 = report (point list2) "second argument must be a list"
  | otherwise          = return $ list (fromList list1 ++ fromList list2)
biAppend _ = reportUndef "two arguments required"

builtinFunctions = [("list",   Nothing,          biList)
                   ,("head",   Just (1 :: Int),  biHead)
                   ,("tail",   Just 1,           biTail)
                   ,("append", Just 2,           biAppend)]

specialOperators = []
