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
import LispShow

data Atom a where
  AInt      ::         Int            -> Atom a
  AFloat    ::         Float          -> Atom a
  AString   ::         String         -> Atom a
  AChar     ::         Char           -> Atom a
  ABool     ::         Bool           -> Atom a
  ASymbol   ::         String         -> Atom a
  ACallable :: Eq a => Callable a     -> Atom a
  AEnv      :: Eq a => Map String a   -> Atom a

instance (LispShow a) => LispShow (Atom a) where
  lisp_show (AInt i)      = show i
  lisp_show (AFloat f)    = show f
  lisp_show (AString s)   = show s
  lisp_show (AChar c)     = show c
  lisp_show (ABool b)     = show b
  lisp_show (ASymbol s)   = '\'' : s
  lisp_show (ACallable c) = '{' : lisp_show c ++ "}"
  lisp_show (AEnv e)      = '{' : lisp_show e ++ "}"

instance LispShow a => LispShow (Map String a) where
  lisp_show = unlines . Map.foldMapWithKey (\key value -> [key ++ " => " ++ lisp_show value])

instance Eq (Atom a) where
  (AInt i)      == (AInt i')      = i == i'
  (AFloat f)    == (AFloat f')    = f == f'
  (AString s)   == (AString s')   = s == s'
  (AChar c)     == (AChar c')     = c == c'
  (ABool b)     == (ABool b')     = b == b'
  (ASymbol s)   == (ASymbol s')   = s == s'
  (ACallable c) == (ACallable c') = undefined
  (AEnv e)      == (AEnv e')      = e == e'
  _             == _              = False

instance Ord (Atom a) where
  compare (AInt i)      (AInt i')      = compare i i'
  compare (AFloat f)    (AFloat f')    = compare f f'
  compare (AString s)   (AString s')   = compare s s'
  compare (AChar c)     (AChar c')     = compare c c'
  compare (ABool b)     (ABool b')     = compare b b'
  compare (ASymbol s)   (ASymbol s')   = compare s s'
  compare (ACallable _) (ACallable _') = undefined
  compare (AEnv e)      (AEnv e')      = undefined
  compare _             _              = undefined

is_int :: Atom a -> Bool
is_int (AInt _) = True
is_int _        = False

from_int :: Atom a -> Int
from_int (AInt i) = i
from_int _        = undefined

is_float :: Atom a -> Bool
is_float (AFloat _) = True
is_float _          = False

from_float :: Atom a -> Float
from_float (AFloat f) = f
from_float _          = undefined

is_number :: Atom a -> Bool
is_number (AInt _)   = True
is_number (AFloat _) = True
is_number _          = False

from_number :: Atom a -> Float
from_number (AInt i)   = fromIntegral i
from_number (AFloat f) = f
from_number _          = undefined

is_string :: Atom a -> Bool
is_string (AString _) = True
is_string _           = False

from_string :: Atom a -> String
from_string (AString s) = s
from_string _           = undefined

is_char :: Atom a -> Bool
is_char (AChar _) = True
is_char _         = False

from_char :: Atom a -> Char
from_char (AChar c) = c
from_char _         = undefined

is_bool :: Atom a -> Bool
is_bool (ABool _) = True
is_bool _         = False

from_bool :: Atom a -> Bool
from_bool (ABool b) = b
from_bool _         = undefined

is_symbol :: Atom a -> Bool
is_symbol (ASymbol _) = True
is_symbol _           = undefined

from_symbol :: Atom a -> String
from_symbol (ASymbol s) = s
from_symbol _           = undefined

is_callable :: Atom a -> Bool
is_callable (ACallable _) = True
is_callable _             = False

from_callable :: Atom a -> Callable a
from_callable (ACallable c) = c
from_callable _             = undefined

is_env :: Atom a -> Bool
is_env (AEnv _) = True
is_env _        = False

from_env :: Atom a -> Map String a
from_env (AEnv e) = e
from_env _        = undefined

str2atom :: String -> Atom a
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

atom_type :: Atom a -> String
atom_type (AInt _)      = "Int"
atom_type (AFloat _)    = "Float"
atom_type (AString _)   = "String"
atom_type (AChar _)     = "Char"
atom_type (ABool _)     = "Bool"
atom_type (ASymbol _)   = "Symbol"
atom_type (ACallable _) = "Callable"
atom_type (AEnv _)      = "Env"
