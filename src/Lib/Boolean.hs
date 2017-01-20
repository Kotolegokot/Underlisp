module Lib.Boolean (builtinFunctions
                   ,specialOperators) where

import Control.Monad (foldM)
import Control.Conditional (ifM, (<&&>), (<||>))
import Base
import Evaluator

default (Int)

biNot :: [SExpr] -> Lisp SExpr
biNot [SAtom p (ABool b)] = return $ SAtom p (ABool $ not b)
biNot [other]             = reportE (point other) "boolean expected"
biNot _                   = reportE' "just one argument required"

soAnd :: [SExpr] -> Lisp SExpr
soAnd = (bool <$>) . foldM (\acc x -> return acc <&&> (getBool =<< evalAlone x)) True

soOr :: [SExpr] -> Lisp SExpr
soOr = (bool <$>) . foldM (\acc x -> return acc <||> (getBool =<< evalAlone x)) False

soImpl :: [SExpr] -> Lisp SExpr
soImpl [arg1, arg2] = ifM (getBool =<< evalAlone arg1) (evalAlone arg2) (return $ bool True)
soImpl _            = reportE' "two arguments requried"

builtinFunctions = [("not", Just 1,  biNot)]
specialOperators = [("and", Nothing, soAnd)
                   ,("or",  Nothing, soOr)
                   ,("->",  Just 2,  soImpl)]
