module SExpr (SExpr (..), str2atom) where

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

isList :: SExpr -> Bool
isList (SList _) = True
isList _         = False

fromList :: SExpr -> [SExpr]
fromList (SList list) = list
fromList _            = error "list expected"

isInt :: SExpr -> Bool
isInt (SInt _) = True
isInt _        = False

fromInt :: SExpr -> Int
fromInt (SInt int) = int
fromInt _          = error "int expected"

isFloat :: SExpr -> Bool
isFloat (SFloat _) = True
isFloat _          = False

fromFloat :: SExpr -> Float
fromFloat (SFloat float) = float
fromFloat _              = error "float expected"

fromNumber :: SExpr -> Float
fromNumber (SFloat float) = float
fromNumber (SInt int)     = fromIntegral int
fromNumber _              = error "int or float expected"

isString :: SExpr -> Bool
isString (SString _) = True
isString _           = False

fromString :: SExpr -> String
fromString (SString string) = string
fromString _                = error "string expected"

isChar :: SExpr -> Bool
isChar (SChar _) = True
isChar _         = False

fromChar :: SExpr -> Char
fromChar (SChar char) = char
fromChar _            = error "char expected"

isBool :: SExpr -> Bool
isBool (SBool _) = True
isBool _         = False

fromBool :: SExpr -> Bool
fromBool (SBool bool) = bool
fromBool _            = error "bool expected"

isKeyword :: SExpr -> Bool
isKeyword (SKeyword _) = True
isKeyword _            = False

fromKeyword :: SExpr -> String
fromKeyword (SKeyword keyword) = keyword
fromKeyword _                  = error "keyword expected"

str2atom :: String -> SExpr
str2atom "True"  = SBool True
str2atom "False" = SBool False
str2atom atom
  | isJust try_int    = SInt    $ read atom
  | isJust try_float  = SFloat  $ read atom
  | isJust try_char   = SChar   $ read atom
  | isJust try_string = SString $ read atom
  where try_int    = readMaybe atom :: Maybe Int
        try_float  = readMaybe atom :: Maybe Float
        try_char   = readMaybe atom :: Maybe Char
        try_string = readMaybe atom :: Maybe String
