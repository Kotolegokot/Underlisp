module SExpr (
    SExpr (..),
    FExpr (..),
    Function (..),
    Context,
    apply,
    str2atom,
    sexpr2fexpr,
    show_sexpr, show_type,
    is_list, from_list, empty_list,
    is_int, from_int,
    is_float, from_float,
    from_number,
    is_string, from_string,
    is_char, from_char,
    is_bool, from_bool,
    is_keyword, from_keyword,
    is_func, from_func) where

import Text.Read
import Data.Maybe
import qualified Data.Map as Map

type Context = Map.Map String SExpr

data Function = UserDefined Int FExpr | BuiltIn String ((Context -> SExpr -> IO (SExpr, Context)) -> Context ->[SExpr] -> IO (SExpr, Context))

instance Eq Function where
    (==) (UserDefined a b) (UserDefined c d) = (a == c) && (b == d)
    (==) (BuiltIn a _)     (BuiltIn b _)     = a == b
    (==) _                 _                 = False

instance Show Function where
    show (UserDefined a b) = "UserDefined " ++ show a ++ " " ++ show b
    show (BuiltIn a _)     = "Built-In Function " ++ a

is_builtin :: Function -> Bool
is_builtin (UserDefined _ _) = True
is_builtin (BuiltIn _ _)     = False

data FExpr = FList [FExpr] | FInt Int | FFloat Float | FString String | FChar Char | FBool Bool | FKeyword String | FFunc Function | FRef Int
  deriving (Eq, Show)

apply :: Function -> [SExpr] -> SExpr
apply (UserDefined args_count fexpr) args = fexpr2sexpr fexpr
  where fexpr2sexpr (FList flist)    = SList $ fmap fexpr2sexpr flist
        fexpr2sexpr (FInt int)       = SInt int
        fexpr2sexpr (FFloat float)   = SFloat float
        fexpr2sexpr (FString string) = SString string
        fexpr2sexpr (FChar char)     = SChar char
        fexpr2sexpr (FBool bool)     = SBool bool
        fexpr2sexpr (FKeyword kword) = SKeyword kword
        fexpr2sexpr (FFunc func)     = SFunc func
        fexpr2sexpr (FRef index)     = args !! index
apply (BuiltIn _ _)                  _    = error "can't apply a built-in function"

sexpr2fexpr :: SExpr -> FExpr
sexpr2fexpr (SList list)     = FList $ fmap sexpr2fexpr list
sexpr2fexpr (SInt int)       = FInt int
sexpr2fexpr (SFloat float)   = FFloat float
sexpr2fexpr (SString string) = FString string
sexpr2fexpr (SChar char)     = FChar char
sexpr2fexpr (SBool bool)     = FBool bool
sexpr2fexpr (SKeyword str)   = FKeyword str
sexpr2fexpr (SFunc func)     = FFunc func

data SExpr = SList [SExpr] | SInt Int | SFloat Float | SString String | SChar Char | SBool Bool | SKeyword String | SFunc Function
  deriving (Eq, Show)

instance Ord SExpr where
    compare (SList a)    (SList b)    = compare a b
    compare (SInt a)     (SInt b)     = compare a b
    compare (SFloat a)   (SFloat b)   = compare a b
    compare (SString a)  (SString b)  = compare a b
    compare (SChar a)    (SChar b)    = compare a b
    compare (SBool a)    (SBool b)    = compare a b
    compare (SKeyword a) (SKeyword b) = compare a b
    compare (SFunc a)    (SFunc b)    = error "can't compare two functions"
    compare _ _ = error "can't compare s-expressions of different types"

show_sexpr :: SExpr -> String
show_sexpr (SList list)       = "(" ++ show_list list ++ ")"
    where show_list [x]    = show_sexpr x
          show_list (x:xs) = show_sexpr x ++ " " ++ show_list xs
          show_list []     = ""
show_sexpr (SInt int)         = show int
show_sexpr (SFloat float)     = show float
show_sexpr (SString string)   = string
show_sexpr (SChar char)       = [char]
show_sexpr (SBool bool)       = show bool
show_sexpr (SKeyword keyword) = keyword
show_sexpr (SFunc func)       = show func

show_type :: SExpr -> String
show_type (SList _)    = "List"
show_type (SInt _)     = "Int"
show_type (SFloat _)   = "Float"
show_type (SString _)  = "String"
show_type (SChar _)    = "Char"
show_type (SBool _)    = "Bool"
show_type (SKeyword _) = "Keyword"

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

is_func :: SExpr -> Bool
is_func (SFunc _) = True
is_func _         = False

from_func :: SExpr -> Function
from_func (SFunc f) = f
from_func _         = error "function expected"

str2atom :: String -> SExpr
str2atom atom
  | isJust try_int    = SInt     $ fromJust try_int
  | isJust try_float  = SFloat   $ fromJust try_float
  | isJust try_char   = SChar    $ fromJust try_char
  | isJust try_string = SString  $ fromJust try_string
  | isJust try_bool   = SBool    $ fromJust try_bool
  | otherwise         = SKeyword atom
  where try_int    = readMaybe atom :: Maybe Int
        try_float  = readMaybe atom :: Maybe Float
        try_char   = readMaybe atom :: Maybe Char
        try_string = readMaybe atom :: Maybe String
        try_bool   = readMaybe atom :: Maybe Bool
