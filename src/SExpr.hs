{-# LANGUAGE GADTs #-}

module SExpr (Expr (..),
              SExpr (..),
              Callable (..),
              Context,
              bind,
              str2atom,
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
  deriving (Eq, Show)

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

class (Eq a, Show a, Ord a) => Expr a where
    is_list       :: a -> Bool
    from_list     :: a -> [a]
    is_int        :: a -> Bool
    from_int      :: a -> Int
    is_float      :: a -> Bool
    from_float    :: a -> Float
    is_number     :: a -> Bool
    from_number   :: a -> Float
    is_string     :: a -> Bool
    from_string   :: a -> String
    is_char       :: a -> Bool
    from_char     :: a -> Char
    is_bool       :: a -> Bool
    from_bool     :: a -> Bool
    is_symbol     :: a -> Bool
    from_symbol   :: a -> String
    is_callable   :: a -> Bool
    from_callable :: a -> Callable
    empty_list    :: a

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
show_type (SList _)    = "List"
show_type (SInt _)     = "Int"
show_type (SFloat _)   = "Float"
show_type (SString _)  = "String"
show_type (SChar _)    = "Char"
show_type (SBool _)    = "Bool"
show_type (SSymbol _)  = "Symbol"

instance Expr SExpr where
    is_list (SList _) = True
    is_list _         = False

    from_list (SList list) = list
    from_list _            = error "list expected"

    empty_list = SList []

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
  -- arg names -> rest -> s-expression -> bound args
  UserDefined :: [String] -> Bool -> SExpr -> [SExpr] -> Callable
  -- arg names -> rest -> s-expression -> bound args
  Macro       :: [String] -> Bool -> SExpr -> [SExpr] -> Callable
  -- name -> function -> bound args
  BuiltIn     :: String -> ([SExpr] -> IO SExpr) -> [SExpr] -> Callable
  -- name -> function -> bound args
  SpecialOp   :: String -> (Eval -> Context -> [SExpr] -> IO (SExpr, Context)) -> [SExpr] -> Callable

instance Eq Callable where
  (==) (UserDefined a1 b1 c1 d1)
       (UserDefined a2 b2 c2 d2)  = (a1 == a2) && (b1 == b2) && (c1 == c2) && (d1 == d2)
  (==) (Macro a1 b1 c1 d1)
       (Macro a2 b2 c2 d2)        = (a1 == a2) && (b1 == b2) && (c1 == c2) && (d1 == d2)
  (==) (BuiltIn name1 _ bound1)
       (BuiltIn name2 _ bound2)   = (name1 == name2) && (bound1 == bound2)
  (==) (SpecialOp name1 _ bound1)
       (SpecialOp name2 _ bound2) = (name1 == name2) && (bound1 == bound2)
  (==) _ _                        = False

instance Show Callable where
  show (UserDefined args rest sexpr bound)  = "User-defined function (args: "
                                                ++ show args ++ ", &rest: " ++ (if rest then "on" else "off")
                                                ++ ", sexpr: " ++ show_sexpr sexpr
                                                ++ ", bound args: " ++ show_sexpr (SList bound) ++ ")"
  show (Macro args rest sexpr bound)        = "Macro (args: " ++ show args
                                                ++ ", &rest: " ++ (if rest then "on" else "off")
                                                ++ ", sexpr: " ++ show_sexpr sexpr
                                                ++ ", bound args: " ++ show_sexpr (SList bound) ++ ")"
  show (BuiltIn name _ bound)               = "Built-in function (name: '" ++ name ++ "'"
                                                ++ ", bound args: " ++ show_sexpr (SList bound) ++ ")"
  show (SpecialOp name _ bound)             = "Special operator (name: '" ++ name ++ "'"
                                                ++ ", bound args: " ++ show_sexpr (SList bound) ++ ")"

bind :: Callable -> [SExpr] -> Callable
bind (UserDefined arg_names False sexpr bound) args
  | length arg_names < (length bound + length args) = error "too many arguments"
  | otherwise                                       = UserDefined arg_names False sexpr (bound ++ args)
bind (UserDefined arg_names True sexpr bound) args = UserDefined arg_names True sexpr (bound ++ args)
bind (Macro arg_names False sexpr bound) args
  | length arg_names < (length bound + length args) = error "too many arguments"
  | otherwise                                       = Macro arg_names False sexpr (bound ++ args)
bind (Macro arg_names True sexpr bound) args = Macro arg_names True sexpr (bound ++ args)
bind (BuiltIn name f bound) args             = BuiltIn name f (bound ++ args)
bind (SpecialOp name f bound) args           = SpecialOp name f (bound ++ args)
