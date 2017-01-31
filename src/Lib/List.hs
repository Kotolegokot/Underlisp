module Lib.List (specialOperators) where

import Control.Monad (liftM)
import Data.IORef

-- local modules
import Base
import Evaluator

default (Int)

biList :: IORef Scope -> [SExpr] -> EvalM SExpr
biList _ = return . list

biHead :: IORef Scope -> [SExpr] -> EvalM SExpr
biHead _ [SList _ (first:_)] = return first
biHead _ [SList p []]        = reportE p "empty list"
biHead _ [sexpr]             = reportE (point sexpr) "list expected"
biHead _ _                   = reportE' "just one argument required"

biTail :: IORef Scope -> [SExpr] -> EvalM SExpr
biTail _ [SList p (_:rest)] = return $ SList p rest
biTail _ [SList p []]       = reportE p "empty list"
biTail _ [sexpr]            = reportE (point sexpr) "list expected"
biTail _ _                  = reportE' "just one argument required"

biAppend :: IORef Scope -> [SExpr] -> EvalM SExpr
biAppend _ = liftM (list . concat) . mapM getList

specialOperators = [("list",   Nothing, withEvaluatedArgs biList)
                   ,("head",   Just 1,  withEvaluatedArgs biHead)
                   ,("tail",   Just 1,  withEvaluatedArgs biTail)
                   ,("append", Just 2,  withEvaluatedArgs biAppend)]
