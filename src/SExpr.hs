{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SExpr (SExpr (..)
             , module Expr
             , module Point
             , Atom (..)
             , str2atom
             , point
             , replace_point) where

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Env
import Env (Env)

import Atom
import Callable
import Expr
import LispShow
import LexicalEnvironment
import Point

-- s-expression --
data SExpr = SList Point [SExpr] | SAtom Point (Atom LEnv SExpr)
  deriving Ord

instance Eq SExpr where
  SList _ s == SList _ s' = s == s'
  SAtom _ a == SAtom _ a' = a == a'
  _         == _          = False

instance Expr LEnv SExpr where
  is_list (SList _ _)   = True
  is_list _             = False

  from_list (SList _ l) = l
  from_list _           = undefined

  from_atom (SAtom _ a) = a
  from_atom _           = undefined

  atom                  = SAtom Undefined
  list                  = SList Undefined

instance LispShow SExpr where
  lisp_show (SList _ xs) = lisp_show xs
  lisp_show (SAtom _ a)  = lisp_show a

point :: SExpr -> Point
point (SList p _) = p
point (SAtom p _) = p

replace_point :: SExpr -> Point -> SExpr
replace_point (SList _ s) p = SList p s
replace_point (SAtom _ a) p = SAtom p a
