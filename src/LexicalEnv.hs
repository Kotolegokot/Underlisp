{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LexicalEnv where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Env
import Expr
import Callable

data LEnv a = LEnv [Map String a]
  deriving (Eq, Functor, Foldable, Traversable)

lreplace :: LEnv a -> Map String a -> LEnv a
lreplace (LEnv (_:xs)) x' = LEnv (x' : xs)
lreplace (LEnv [])     _  = undefined

xadd :: LEnv a -> Map String a -> LEnv a
xadd (LEnv (x:xs)) x' = LEnv (x : x' : xs)
xadd (LEnv [])     x' = undefined

instance (Expr LEnv a, Eq a) => Env LEnv a where
  empty          = LEnv [Map.empty]
  fromList l     = LEnv [Map.fromList l]
  pass (LEnv xs) = LEnv (Map.empty : xs)

  linsert key value (LEnv (x:xs)) = LEnv (x' : xs)
    where x' = fmap (\sexpr -> if is_callable sexpr
                               then case from_callable sexpr of
                                      UserDefined e prototype sexprs bound
                                            -> callable $ UserDefined (lreplace e (x' `Map.union` (lexical e))) prototype sexprs bound
                                      other -> callable other
                               else sexpr)
               (Map.insert key value x)
  linsert _   _     (LEnv [])     = undefined

  xinsert key value (LEnv (x:xs)) = LEnv (x' : ext : xs)
    where ext = Map.fromList [(key, value)]
          x' = fmap (\sexpr -> if is_callable sexpr
                               then case from_callable sexpr of
                                      UserDefined e prototype sexprs bound
                                            -> callable $ UserDefined (xadd e (Map.insert key value $ external e)) prototype sexprs bound
                                      other -> callable other
                               else sexpr)
               x
  xinsert _   _     (LEnv [])     = undefined

  lappend (LEnv (x:xs)) add = LEnv (x' : xs)
    where x' = fmap (\sexpr -> if is_callable sexpr
                               then case from_callable sexpr of
                                      UserDefined e prototype sexprs bound
                                            -> callable $ UserDefined (lreplace e $ x' `Map.union` (lexical e)) prototype sexprs bound
                                      other -> callable other
                               else sexpr)
               (add `Map.union` x)
  lappend (LEnv [])     _   = undefined

  xappend (LEnv (x:xs)) add = LEnv (x' : add : xs)
    where x' = fmap (\sexpr -> if is_callable sexpr
                               then case from_callable sexpr of
                                      UserDefined e prototype sexprs bound
                                            -> callable $ UserDefined (xadd e $ add `Map.union` (external e)) prototype sexprs bound
                                      other -> callable other
                               else sexpr)
               x
  xappend (LEnv [])     _   = undefined

  lexical (LEnv (x:_)) = x
  external (LEnv (_:xs)) = Map.unions xs
