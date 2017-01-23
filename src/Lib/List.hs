module Lib.List (builtinFunctions
                ,specialOperators) where

import Control.Monad (liftM)
import Data.IORef
import Base

default (Int)

biList :: IORef Scope -> [SExpr] -> Lisp SExpr
biList _ = return . list

biHead :: IORef Scope -> [SExpr] -> Lisp SExpr
biHead _ [SList _ (first:_)] = return first
biHead _ [SList p []]        = reportE p "empty list"
biHead _ [sexpr]             = reportE (point sexpr) "list expected"
biHead _ _                   = reportE' "just one argument required"

biTail :: IORef Scope -> [SExpr] -> Lisp SExpr
biTail _ [SList p (_:rest)] = return $ SList p rest
biTail _ [SList p []]       = reportE p "empty list"
biTail _ [sexpr]            = reportE (point sexpr) "list expected"
biTail _ _                  = reportE' "just one argument required"

biAppend :: IORef Scope -> [SExpr] -> Lisp SExpr
biAppend _ = liftM (list . concat) . mapM getList

builtinFunctions = [("list",   Nothing, biList)
                   ,("head",   Just 1,  biHead)
                   ,("tail",   Just 1,  biTail)
                   ,("append", Just 2,  biAppend)]

specialOperators = []
