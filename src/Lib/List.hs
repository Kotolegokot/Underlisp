module Lib.List (builtinList,
                 builtinHead,
                 builtinTail,
                 builtinNull,
                 builtinAppend) where

import SExpr
import Exception

builtinList :: [SExpr] -> IO SExpr
builtinList = return . list

builtinHead :: [SExpr] -> IO SExpr
builtinHead [SList _ (first:_)] = return first
builtinHead [SList p []]        = report p "empty list"
builtinHead [sexpr]             = report (point sexpr) "list expected"
builtinHead _                   = reportUndef "just one argument required"

builtinTail :: [SExpr] -> IO SExpr
builtinTail [SList p (_:rest)] = return $ SList p rest
builtinTail [SList p []]       = report p "empty list"
builtinTail [sexpr]            = report (point sexpr) "list expected"
builtinTail _                  = reportUndef "just one argument required"

builtinNull :: [SExpr] -> IO SExpr
builtinNull [SList p list]    = return . bool . null $ list
builtinNull [sexpr]           = report (point sexpr) "list expected"
builtinNull _                 = reportUndef "just one argument requried"

builtinAppend :: [SExpr] -> IO SExpr
builtinAppend [list1, list2]
  | not $ isList list1 = report (point list1) "first argument must be a list"
  | not $ isList list2 = report (point list2) "second argument must be a list"
  | otherwise          = return $ list (fromList list1 ++ fromList list2)
builtinAppend _ = reportUndef "two arguments required"
