{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
module Atom where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Text.Read (readMaybe)
import Callable
import Env
import LispShow

data Atom e a where
  AInt      ::                    Int              -> Atom e a
  AFloat    ::                    Float            -> Atom e a
  AString   ::                    String           -> Atom e a
  AChar     ::                    Char             -> Atom e a
  ABool     ::                    Bool             -> Atom e a
  ASymbol   ::                    String           -> Atom e a
  ACallable :: (Eq a, Env e a) => Callable e a     -> Atom e a
  AEnv      :: Eq a =>            Map String a     -> Atom e a

instance (LispShow a) => LispShow (Atom e a) where
  lisp_show (AInt i)      = show i
  lisp_show (AFloat f)    = show f
  lisp_show (AString s)   = show s
  lisp_show (AChar c)     = show c
  lisp_show (ABool b)     = show b
  lisp_show (ASymbol s)   = s
  lisp_show (ACallable c) = '{' : lisp_show c ++ "}"
  lisp_show (AEnv e)      = '{' : lisp_show e ++ "}"

instance LispShow a => LispShow (Map String a) where
  lisp_show map = unlines $ Map.foldMapWithKey (\key value -> [key ++ space key ++ " => " ++ lisp_show value]) map
    where indent    = maximum . fmap length $ Map.keys map
          space key = replicate (indent - length key) ' '

instance Eq (Atom e a) where
  (AInt i)      == (AInt i')      = i == i'
  (AFloat f)    == (AFloat f')    = f == f'
  (AString s)   == (AString s')   = s == s'
  (AChar c)     == (AChar c')     = c == c'
  (ABool b)     == (ABool b')     = b == b'
  (ASymbol s)   == (ASymbol s')   = s == s'
  (ACallable c) == (ACallable c') = undefined
  (AEnv e)      == (AEnv e')      = e == e'
  _             == _              = False

instance Ord (Atom e a) where
  compare (AInt i)      (AInt i')      = compare i i'
  compare (AFloat f)    (AFloat f')    = compare f f'
  compare (AString s)   (AString s')   = compare s s'
  compare (AChar c)     (AChar c')     = compare c c'
  compare (ABool b)     (ABool b')     = compare b b'
  compare (ASymbol s)   (ASymbol s')   = compare s s'
  compare (ACallable _) (ACallable _') = undefined
  compare (AEnv e)      (AEnv e')      = undefined
  compare _             _              = undefined

is_int :: Atom e a -> Bool
is_int (AInt _) = True
is_int _        = False

from_int :: Atom e a -> Int
from_int (AInt i) = i
from_int _        = undefined

is_float :: Atom e a -> Bool
is_float (AFloat _) = True
is_float _          = False

from_float :: Atom e a -> Float
from_float (AFloat f) = f
from_float _          = undefined

is_number :: Atom e a -> Bool
is_number (AInt _)   = True
is_number (AFloat _) = True
is_number _          = False

from_number :: Atom e a -> Float
from_number (AInt i)   = fromIntegral i
from_number (AFloat f) = f
from_number _          = undefined

is_string :: Atom e a -> Bool
is_string (AString _) = True
is_string _           = False

from_string :: Atom e a -> String
from_string (AString s) = s
from_string _           = undefined

is_char :: Atom e a -> Bool
is_char (AChar _) = True
is_char _         = False

from_char :: Atom e a -> Char
from_char (AChar c) = c
from_char _         = undefined

is_bool :: Atom e a -> Bool
is_bool (ABool _) = True
is_bool _         = False

from_bool :: Atom e a -> Bool
from_bool (ABool b) = b
from_bool _         = undefined

is_symbol :: Atom e a -> Bool
is_symbol (ASymbol _) = True
is_symbol _           = undefined

from_symbol :: Atom e a -> String
from_symbol (ASymbol s) = s
from_symbol _           = undefined

is_callable :: Atom e a -> Bool
is_callable (ACallable _) = True
is_callable _             = False

from_callable :: Atom e a -> Callable e a
from_callable (ACallable c) = c
from_callable _             = undefined

is_env :: Atom e a -> Bool
is_env (AEnv _) = True
is_env _        = False

from_env :: Atom e a -> Map String a
from_env (AEnv e) = e
from_env _        = undefined

str2atom :: String -> Atom e a
str2atom atom
  | isJust try_int    = AInt     $ fromJust try_int
  | isJust try_float  = AFloat   $ fromJust try_float
  | isJust try_char   = AChar    $ fromJust try_char
  | isJust try_string = AString  $ fromJust try_string
  | isJust try_bool   = ABool    $ fromJust try_bool
  | otherwise         = ASymbol atom
  where try_int    = readMaybe atom :: Maybe Int
        try_float  = readMaybe atom :: Maybe Float
        try_char   = readMaybe atom :: Maybe Char
        try_string = readMaybe atom :: Maybe String
        try_bool   = readMaybe atom :: Maybe Bool

atom_type :: Atom e a -> String
atom_type (AInt _)      = "Int"
atom_type (AFloat _)    = "Float"
atom_type (AString _)   = "String"
atom_type (AChar _)     = "Char"
atom_type (ABool _)     = "Bool"
atom_type (ASymbol _)   = "Symbol"
atom_type (ACallable _) = "Callable"
atom_type (AEnv _)      = "Env"
