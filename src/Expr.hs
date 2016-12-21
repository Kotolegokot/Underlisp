{-# LANGUAGE MultiParamTypeClasses  #-}
module Expr where

import qualified Atom as A
import Atom
import Callable
import Data.Map (Map)

class Expr a where
  is_list   :: a -> Bool
  is_list = not . is_atom

  is_atom   :: a -> Bool
  is_atom = not . is_list

  from_list :: a -> [a]
  from_atom :: a -> Atom a

  nil       :: a
  atom      :: Atom a -> a
  list      :: [Atom a] -> a

  {-# MINIMAL from_list, from_atom, nil, atom, list, (is_list | is_atom) #-}

expr_type :: Expr a => a -> String
expr_type x = if is_list x
              then "List"
              else atom_type $ from_atom x

int :: Expr a => Int -> a
int = atom . AInt

float :: Expr a => Float -> a
float = atom . AFloat

string :: Expr a => String -> a
string = atom . AString

char :: Expr a => Char -> a
char = atom . AChar

bool :: Expr a => Bool -> a
bool  = atom . ABool

symbol :: Expr a => String -> a
symbol = atom . ASymbol

callable :: (Expr a, Eq a) => Callable a -> a
callable = atom . ACallable

env :: (Expr a, Eq a) => Map String a -> a
env = atom . AEnv

is_int :: Expr a => a -> Bool
is_int = A.is_int . from_atom

from_int :: Expr a => a -> Int
from_int = A.from_int . from_atom

is_float :: Expr a => a -> Bool
is_float = A.is_float . from_atom

from_float :: Expr a => a -> Float
from_float = A.from_float . from_atom

is_number :: Expr a => a -> Bool
is_number = A.is_number . from_atom

from_number :: Expr a => a -> Float
from_number = A.from_number . from_atom

is_string :: Expr a => a -> Bool
is_string = A.is_string . from_atom

from_string :: Expr a => a -> String
from_string = A.from_string . from_atom

is_char :: Expr a => a -> Bool
is_char = A.is_char . from_atom

from_char :: Expr a => a -> Char
from_char = A.from_char . from_atom

is_bool :: Expr a => a -> Bool
is_bool = A.is_bool . from_atom

from_bool :: Expr a => a -> Bool
from_bool = A.from_bool . from_atom

is_symbol :: Expr a => a -> Bool
is_symbol = A.is_symbol . from_atom

from_symbol :: Expr a => a -> String
from_symbol = A.from_symbol . from_atom

is_callable :: Expr a => a -> Bool
is_callable = A.is_callable . from_atom

from_callable :: Expr a => a -> Callable a
from_callable = A.from_callable . from_atom

is_env :: Expr a => a -> Bool
is_env = A.is_env . from_atom

from_env :: Expr a => a -> Map String a
from_env = A.from_env . from_atom
