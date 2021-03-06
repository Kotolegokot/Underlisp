module Lib.Boolean (specialOperators) where

import Data.IORef
import Control.Monad (foldM)
import Control.Conditional (ifM, (<&&>), (<||>))
import Base
import Evaluator

default (Int)

biNot :: IORef Scope -> [SExpr] -> EvalM SExpr
biNot _ [SAtom p (ABool b)] = return $ SAtom p (ABool $ not b)
biNot _ [other]             = reportE (point other) "boolean expected"
biNot _ _                   = reportE' "just one argument required"

soAnd :: IORef Scope -> [SExpr] -> EvalM SExpr
soAnd scopeRef = fmap bool . foldM (\acc x -> return acc <&&> (getBool =<< evalAlone scopeRef x)) True

soOr :: IORef Scope -> [SExpr] -> EvalM SExpr
soOr scopeRef = fmap bool . foldM (\acc x -> return acc <||> (getBool =<< evalAlone scopeRef x)) False

soImpl :: IORef Scope -> [SExpr] -> EvalM SExpr
soImpl scopeRef [arg1, arg2] = ifM (getBool =<< evalAlone scopeRef arg1) (evalAlone scopeRef arg2) (return $ bool True)
soImpl _        _            = reportE' "two arguments requried"

specialOperators = [("and", Nothing, soAnd)
                   ,("or",  Nothing, soOr)
                   ,("->",  Just 2,  soImpl)
                   ,("not", Just 1,  withEvaluatedArgs biNot)]
