{-# LANGUAGE GADTs #-}

module SExpr (SExpr (..),
              Callable (..),
              Context,
              bind,
              str2atom,
              nil,
              is_list, from_list,
              is_int, from_int,
              is_float, from_float,
              is_number, from_number,
              is_string, from_string,
              is_char, from_char,
              is_bool, from_bool,
              is_symbol, from_symbol,
              is_callable, from_callable,
              is_context, from_context,
              show_sexpr, show_type) where

import Data.Maybe
import Text.Read (readMaybe)
import qualified Data.Map as Map
import Data.Map (Map)

type Context = Map String SExpr

-- SExpr --
data SExpr = SList     [SExpr]
           | SInt      Int
           | SFloat    Float
           | SString   String
           | SChar     Char
           | SBool     Bool
           | SSymbol   String
           | SCallable Callable
           | SContext  Context
  deriving (Show)

instance Eq SExpr where
  (==) (SList list1)     (SList list2)     = list1 == list2
  (==) (SInt int1)       (SInt int2)       = int1 == int2
  (==) (SFloat float1)   (SFloat float2)   = float1 == float2
  (==) (SString string1) (SString string2) = string1 == string2
  (==) (SChar char1)     (SChar char2)     = char1 == char2
  (==) (SBool bool1)     (SBool bool2)     = bool1 == bool2
  (==) (SSymbol sym1)    (SSymbol sym2)    = sym1 == sym2
  (==) (SCallable _)     _                 = undefined
  (==) _                 (SCallable _)     = undefined
  (==) (SContext _)      _                 = undefined
  (==) _                 (SContext _)      = undefined
  (==) _                 _                 = False

instance Ord SExpr where
    compare (SList a)        (SList b)        = compare a b
    compare (SInt a)         (SInt b)         = compare a b
    compare (SFloat a)       (SFloat b)       = compare a b
    compare (SString a)      (SString b)      = compare a b
    compare (SChar a)        (SChar b)        = compare a b
    compare (SBool a)        (SBool b)        = compare a b
    compare (SSymbol a)      (SSymbol b)      = compare a b
    compare (SCallable a)    (SCallable b)    = error "can't compare two functions"
    compare _                 _               = error "can't compare s-expressions of different types"

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
show_sexpr (SSymbol symbol)   = symbol
show_sexpr (SCallable func)   = show func
show_sexpr (SContext context) = "Context " ++ (show $ Map.toList context)

show_type :: SExpr -> String
show_type (SList _)     = "List"
show_type (SInt _)      = "Int"
show_type (SFloat _)    = "Float"
show_type (SString _)   = "String"
show_type (SChar _)     = "Char"
show_type (SBool _)     = "Bool"
show_type (SSymbol _)   = "Symbol"
show_type (SCallable _) = "Callable"
show_type (SContext _)  = "Context"

is_list (SList _) = True
is_list _         = False

from_list (SList list) = list
from_list _            = error "list expected"

nil = SList []

is_int (SInt _) = True
is_int _        = False

from_int (SInt int) = int
from_int _          = error "int expected"

is_float (SFloat _) = True
is_float _          = False

from_float (SFloat float) = float
from_float _              = error "float expected"

is_number sexpr = is_int sexpr || is_float sexpr

from_number (SFloat float) = float
from_number (SInt int)     = fromIntegral int
from_number _              = error "int or float expected"

is_string (SString _) = True
is_string _           = False

from_string (SString string) = string
from_string _                = error "string expected"

is_char (SChar _) = True
is_char _         = False

from_char (SChar char) = char
from_char _            = error "char expected"

is_bool (SBool _) = True
is_bool _         = False

from_bool (SBool bool) = bool
from_bool _            = error "bool expected"

is_symbol (SSymbol _) = True
is_symbol _           = False

from_symbol (SSymbol keyword) = keyword
from_symbol _                  = error "keyword expected"

is_callable (SCallable _) = True
is_callable _         = False

from_callable (SCallable f) = f
from_callable _         = error "function expected"

is_context (SContext _) = True
is_context _            = False

from_context (SContext c) = c
from_context _            = error "context expected"

str2atom :: String -> SExpr
str2atom atom
  | isJust try_int    = SInt     $ fromJust try_int
  | isJust try_float  = SFloat   $ fromJust try_float
  | isJust try_char   = SChar    $ fromJust try_char
  | isJust try_string = SString  $ fromJust try_string
  | isJust try_bool   = SBool    $ fromJust try_bool
  | otherwise         = SSymbol atom
  where try_int    = readMaybe atom :: Maybe Int
        try_float  = readMaybe atom :: Maybe Float
        try_char   = readMaybe atom :: Maybe Char
        try_string = readMaybe atom :: Maybe String
        try_bool   = readMaybe atom :: Maybe Bool

type Eval = Context -> SExpr -> IO (SExpr, Context)

data Callable where
  -- lexical scope -> arg names -> rest -> s-expression -> bound args
  UserDefined :: Context -> [String] -> Bool -> SExpr -> [SExpr] -> Callable
  -- lexical scope -> arg names -> rest -> s-expression -> bound args
  Macro       :: Context -> [String] -> Bool -> SExpr -> [SExpr] -> Callable
  -- name -> args count or rest -> function -> bound args
  BuiltIn     :: String -> Maybe Int -> ([SExpr] -> IO SExpr) -> [SExpr] -> Callable
  -- name -> args count or rest -> function -> bound args
  SpecialOp   :: String -> Maybe Int -> (Eval -> Context -> [SExpr] -> IO (SExpr, Context)) -> [SExpr] -> Callable

instance Show Callable where
  show (UserDefined _ args rest sexpr bound)  = "User-defined function (args: "
                                                ++ show args ++ ", &rest: " ++ (if rest then "on" else "off")
                                                ++ ", sexpr: " ++ show_sexpr sexpr
                                                ++ ", bound args: " ++ show_sexpr (SList bound) ++ ")"
  show (Macro _ args rest sexpr bound)        = "Macro (args: " ++ show args
                                                ++ ", &rest: " ++ (if rest then "on" else "off")
                                                ++ ", sexpr: " ++ show_sexpr sexpr
                                                ++ ", bound args: " ++ show_sexpr (SList bound) ++ ")"
  show (BuiltIn name _ _ bound)               = "Built-in function (name: '" ++ name ++ "'"
                                                ++ ", bound args: " ++ show_sexpr (SList bound) ++ ")"
  show (SpecialOp name _ _ bound)             = "Special operator (name: '" ++ name ++ "'"
                                                ++ ", bound args: " ++ show_sexpr (SList bound) ++ ")"

bind :: Callable -> [SExpr] -> Callable
bind (UserDefined scope arg_names rest sexpr bound) args
  | rest && length arg_names < (length bound + length args) = error "too many arguments"
  | otherwise                                               = UserDefined scope arg_names rest sexpr (bound ++ args)
bind (Macro scope arg_names rest sexpr bound) args
  | rest && length arg_names < (length bound + length args) = error "too many arguments"
  | otherwise                                               = Macro scope arg_names rest sexpr (bound ++ args)
bind (BuiltIn name (Just args_count) f bound) args
  | args_count < (length bound + length args) = error "too many arguments"
  | otherwise                                 = BuiltIn name (Just args_count) f (bound ++ args)
bind (BuiltIn name Nothing f bound) args = BuiltIn name Nothing f (bound ++ args)
bind (SpecialOp name (Just args_count) f bound) args
  | args_count < (length bound + length args) = error "too many arguments"
  | otherwise                                 = SpecialOp name (Just args_count) f (bound ++ args)
bind (SpecialOp name Nothing f bound) args = SpecialOp name Nothing f (bound ++ args)
