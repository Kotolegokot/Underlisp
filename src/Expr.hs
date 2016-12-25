{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
module Expr where

import qualified Atom as A
import Atom
import Callable
import Data.Map (Map)
import qualified Data.Map as Map
import Env

class Env e a => Expr e a | a -> e where
  is_list   :: a -> Bool
  is_list = not . is_atom

  is_atom   :: a -> Bool
  is_atom = not . is_list

  from_list :: a -> [a]
  from_atom :: a -> Atom e a

  nil       :: a
  atom      :: Atom e a -> a
  list      :: [Atom e a] -> a

  {-# MINIMAL from_list, from_atom, nil, atom, list, (is_list | is_atom) #-}

expr_type :: Expr e a => a -> String
expr_type x = if is_list x
              then "List"
              else atom_type $ from_atom x

int :: Expr e a => Int -> a
int = atom . AInt

float :: Expr e a => Float -> a
float = atom . AFloat

char :: Expr e a => Char -> a
char = atom . AChar

bool :: Expr e a => Bool -> a
bool  = atom . ABool

symbol :: Expr e a => String -> a
symbol = atom . ASymbol

callable :: (Env e a, Expr e a, Eq a) => Callable e a -> a
callable = atom . ACallable

env :: (Expr e a, Eq a) => Map String a -> a
env = atom . AEnv

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = \a -> p1 a && p2 a

infixr 8 .&&

is_int :: Expr e a => a -> Bool
is_int = is_atom .&& A.is_int . from_atom

from_int :: Expr e a => a -> Int
from_int = A.from_int . from_atom

is_float :: Expr e a => a -> Bool
is_float = is_atom .&& A.is_float . from_atom

from_float :: Expr e a => a -> Float
from_float = A.from_float . from_atom

is_number :: Expr e a => a -> Bool
is_number = is_atom .&& A.is_number . from_atom

from_number :: Expr e a => a -> Float
from_number = A.from_number . from_atom

is_char :: Expr e a => a -> Bool
is_char = is_atom .&& A.is_char . from_atom

from_char :: Expr e a => a -> Char
from_char = A.from_char . from_atom

is_bool :: Expr e a => a -> Bool
is_bool = is_atom .&& A.is_bool . from_atom

from_bool :: Expr e a => a -> Bool
from_bool = A.from_bool . from_atom

is_symbol :: Expr e a => a -> Bool
is_symbol = is_atom .&& A.is_symbol . from_atom

from_symbol :: Expr e a => a -> String
from_symbol = A.from_symbol . from_atom

is_callable :: Expr e a => a -> Bool
is_callable = is_atom .&& A.is_callable . from_atom

from_callable :: Expr e a => a -> Callable e a
from_callable = A.from_callable . from_atom

is_env :: Expr e a => a -> Bool
is_env = is_atom .&& A.is_env . from_atom

from_env :: Expr e a => a -> Map String a
from_env = A.from_env . from_atom
