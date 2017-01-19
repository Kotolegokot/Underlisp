module Lib.List (builtinFunctions
                ,specialOperators) where

import Control.Monad (liftM)
import Base

default (Int)

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
biAppend = liftM (list . concat) . mapM getList

builtinFunctions = [("list",   Nothing, biList)
                   ,("head",   Just 1,  biHead)
                   ,("tail",   Just 1,  biTail)
                   ,("append", Just 2,  biAppend)]

specialOperators = []
