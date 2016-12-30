{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SExpr (SExpr (..)
             , module Expr
             , module Point
             , Atom (..)
             , strToAtom
             , point
             , replacePoint) where

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

instance Eq SExpr where
  SList _ s == SList _ s' = s == s'
  SAtom _ a == SAtom _ a' = a == a'
  _         == _          = False

instance Ord SExpr where
  compare (SList _ l) (SList _ l') = compare l l'
  compare (SAtom _ a) (SAtom _ a') = compare a a'
  compare _           _            = undefined

instance Expr LEnv SExpr where
  isList (SList _ _)   = True
  isList _             = False

  fromList (SList _ l) = l
  fromList _           = undefined

  fromAtom (SAtom _ a) = a
  fromAtom _           = undefined

  atom                  = SAtom Undefined
  list                  = SList Undefined

instance LispShow SExpr where
  lispShow (SList _ xs) = lispShow xs
  lispShow (SAtom _ a)  = lispShow a

point :: SExpr -> Point
point (SList p _) = p
point (SAtom p _) = p

replacePoint :: SExpr -> Point -> SExpr
replacePoint (SList _ s) p = SList p s
replacePoint (SAtom _ a) p = SAtom p a
