module SExpr (
    SExpr (..),
    str2atom,
    is_list, from_list, empty_list,
    is_int, from_int,
    is_float, from_float,
    from_number,
    is_string, from_string,
    is_char, from_char,
    is_bool, from_bool,
    is_keyword, from_keyword) where

import Text.Read
import Data.Maybe

data SExpr = SList [SExpr] | SInt Int | SFloat Float | SString String | SChar Char | SBool Bool | SKeyword String
  deriving (Eq, Show)

instance Ord SExpr where
    compare (SInt a)     (SInt b)     = compare a b
    compare (SFloat a)   (SFloat b)   = compare a b
    compare (SString a)  (SString b)  = compare a b
    compare (SChar a)    (SChar b)    = compare a b
    compare (SBool a)    (SBool b)    = compare a b
    compare (SKeyword a) (SKeyword b) = compare a b
    compare _ _ = error "can't compare terminals of different types"

is_list :: SExpr -> Bool
is_list (SList _) = True
is_list _         = False

from_list :: SExpr -> [SExpr]
from_list (SList list) = list
from_list _            = error "list expected"

empty_list :: SExpr
empty_list = SList []

is_int :: SExpr -> Bool
is_int (SInt _) = True
is_int _        = False

from_int :: SExpr -> Int
from_int (SInt int) = int
from_int _          = error "int expected"

is_float :: SExpr -> Bool
is_float (SFloat _) = True
is_float _          = False

from_float :: SExpr -> Float
from_float (SFloat float) = float
from_float _              = error "float expected"

from_number :: SExpr -> Float
from_number (SFloat float) = float
from_number (SInt int)     = fromIntegral int
from_number _              = error "int or float expected"

is_string :: SExpr -> Bool
is_string (SString _) = True
is_string _           = False

from_string :: SExpr -> String
from_string (SString string) = string
from_string _                = error "string expected"

is_char :: SExpr -> Bool
is_char (SChar _) = True
is_char _         = False

from_char :: SExpr -> Char
from_char (SChar char) = char
from_char _            = error "char expected"

is_bool :: SExpr -> Bool
is_bool (SBool _) = True
is_bool _         = False

from_bool :: SExpr -> Bool
from_bool (SBool bool) = bool
from_bool _            = error "bool expected"

is_keyword :: SExpr -> Bool
is_keyword (SKeyword _) = True
is_keyword _            = False

from_keyword :: SExpr -> String
from_keyword (SKeyword keyword) = keyword
from_keyword _                  = error "keyword expected"

str2atom :: String -> SExpr
str2atom "True"  = SBool True
str2atom "False" = SBool False
str2atom atom
  | isJust try_int    = SInt     $ read atom
  | isJust try_float  = SFloat   $ read atom
  | isJust try_char   = SChar    $ read atom
  | isJust try_string = SString  $ read atom
  | otherwise         = SKeyword $ read atom
  where try_int    = readMaybe atom :: Maybe Int
        try_float  = readMaybe atom :: Maybe Float
        try_char   = readMaybe atom :: Maybe Char
        try_string = readMaybe atom :: Maybe String
