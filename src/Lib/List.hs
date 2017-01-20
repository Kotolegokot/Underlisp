module Lib.List (builtinFunctions
                ,specialOperators) where

import Control.Monad (liftM)
import Base

default (Int)

biList :: [SExpr] -> Lisp SExpr
biList = return . list

biHead :: [SExpr] -> Lisp SExpr
biHead [SList _ (first:_)] = return first
biHead [SList p []]        = reportE p "empty list"
biHead [sexpr]             = reportE (point sexpr) "list expected"
biHead _                   = reportE' "just one argument required"

biTail :: [SExpr] -> Lisp SExpr
biTail [SList p (_:rest)] = return $ SList p rest
biTail [SList p []]       = reportE p "empty list"
biTail [sexpr]            = reportE (point sexpr) "list expected"
biTail _                  = reportE' "just one argument required"

biAppend :: [SExpr] -> Lisp SExpr
biAppend = liftM (list . concat) . mapM getList

builtinFunctions = [("list",   Nothing, biList)
                   ,("head",   Just 1,  biHead)
                   ,("tail",   Just 1,  biTail)
                   ,("append", Just 2,  biAppend)]

specialOperators = []
