{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SExpr (SExpr (..)
             , module Expr
             , Atom (..)
             , str2atom) where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Env
import Env (Env)

import Atom
import Callable
import Expr
import LispShow
import LexicalEnvironment

-- s-expression --
data SExpr = SList [SExpr] | SAtom (Atom LEnv SExpr)
  deriving (Eq, Ord)

instance Expr LEnv SExpr where
  is_list (SList _)   = True
  is_list _           = False

  from_list (SList l) = l
  from_list _         = undefined

  from_atom (SAtom a) = a
  from_atom _         = undefined

  nil                 = SList []
  atom                = SAtom
  list                = SList

instance LispShow SExpr where
  lisp_show (SList xs) = lisp_show xs
  lisp_show (SAtom a)  = lisp_show a
