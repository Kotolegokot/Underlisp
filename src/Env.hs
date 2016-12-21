{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE GADTs             #-}

module Env where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

data Env a where
  -- Lexical Scope -> External Scope -> Environment a
  Env :: (Map String a) -> (Map String a) -> Env a
  deriving (Eq, Show, Foldable, Traversable, Functor)

-- | moves the lexical scope to external
pass :: Env a -> Env a
pass (Env lexical external) = Env Map.empty (lexical `Map.union` external)

-- | returns the union of the lexical and external scopes
merge :: Env a -> Map String a
merge (Env lexical external) = lexical `Map.union` external

-- | lookup in both the lexical and external scopes
-- | the lexical one takes priority
lookup :: String -> Env a -> Maybe a
lookup elem (Env lexical external) = case Map.lookup elem lexical of
  Just smth -> Just smth
  Nothing   -> Map.lookup elem external

-- | check if elem is a member of the environment
member :: String -> Env a -> Bool
member elem (Env lexical external) = elem `Map.member` lexical || elem `Map.member` external

-- | puts (key value) pairs to the lexical scope
insert :: String -> a -> Env a -> Env a
insert key value (Env lexical external) = Env (Map.insert key value lexical) external

-- | adds pairs to the lexical scope
append :: Map String a -> Env a -> Env a
append pairs (Env lexical external) = Env (pairs `Map.union` lexical) external

-- | add pairs to the external scope
append_ex :: Map String a -> Env a -> Env a
append_ex pairs (Env lexical external) = Env lexical (pairs `Map.union` external)

-- | adds env' to the external scope
union_ex :: Env a -> Env a -> Env a
union_ex (Env lexical1 external1) env' = Env lexical1 (merge env' `Map.union` external1)

-- | returns empty scope
empty :: Env a
empty = Env Map.empty Map.empty

-- | constructs environment from a list
fromList :: [(String, a)] -> Env a
fromList list = Env Map.empty (Map.fromList list)

-- | returns the lexical scope
get_lexical_scope :: Env a -> Map String a
get_lexical_scope (Env lexical _) = lexical

-- | returns the external scope
get_external_scope :: Env a -> Map String a
get_external_scope (Env _ external) = external
