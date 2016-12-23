{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Env where

import Data.Map (Map)
import qualified Data.Map as Map
import LispShow

class (LispShow (e a), Functor e, Foldable e, Traversable e) => Env e a  where
  -- | environment with empty lexical scope
  empty    :: e a

  -- | creates environment from a list
  fromList :: [(String, a)] -> e a

  -- | moves the lexical scope to external
  pass     :: e a -> e a

  -- | returns the union of the lexical and external scopes
  merge    :: e a -> Map String a
  merge e = lexical e `Map.union` external e

  -- | lookup in both the lexical and external scopes
  -- | the lexical one takes priority
  lookup   :: String -> e a -> Maybe a
  lookup key e = case Map.lookup key (lexical e) of
    Just value -> Just value
    Nothing    -> Map.lookup key (external e)

  -- | check if elem is a member of the environment
  member   :: String -> e a -> Bool
  member key e = key `Map.member` lexical e || key `Map.member` external e

  -- | puts a (key value) pair to the lexical scope
  linsert  :: String -> a -> e a -> e a

  -- | puts a (key value) pair to the external scope
  xinsert  :: String -> a -> e a -> e a

  -- | adds pairs to the lexical scope
  lappend  :: e a -> Map String a -> e a

  -- | adds pairs to the external scope
  xappend  :: e a -> Map String a -> e a

  -- | lexical scope
  lexical  :: e a -> Map String a

  -- | external scope
  external :: e a -> Map String a

  {-# MINIMAL empty, fromList, pass, linsert, xinsert, lappend, xappend, lexical, external #-}
