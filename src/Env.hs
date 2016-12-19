{-# LANGUAGE GADTs #-}

module Env where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

data Env a where
  -- Lexical Scope -> External Scope -> Environment a
  Env :: (Map String a) -> (Map String a) -> Env a

pass_env :: Env a -> Env a
pass_env (Env lexical external) = Env Map.empty (lexical `Map.union` external)

lookup :: Env a -> String -> Maybe a
lookup (Env lexical external) elem = case Map.lookup elem lexical of
  Just smth -> Just smth
  Nothing   -> Map.lookup elem external

member :: Env a -> String -> Bool
member (Env lexical external) elem = elem `Map.member` lexical || elem `Map.member` external

get_lexical_scope :: Env a -> Map String a
get_lexical_scope (Env lexical _) = lexical

get_external_scope :: Env a -> Map String a
get_external_scope (Env _ external) = external
