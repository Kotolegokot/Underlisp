module SemanticAnalyzer
    (Atom(..),
     SExpr,
     analyze,
     isKeyword,
     fromKeyword,
     fromNumber,
     fromFloat,
     fromInt,
     printTerminal,
     printType,
     boolToTerminal,
     terminalToBool) where

import Text.Read
import Data.Maybe
import Data.Tree
import Lexer

data Atom = TInt Int | TFloat Float | TString String | TChar Char | TT | TNil | TKeyword String | TList [Atom]
  deriving (Eq, Show)

type SExpr = Tree Atom

instance Ord Atom where
    compare (TInt a) (TInt b) = compare a b
    compare (TFloat a) (TFloat b) = compare a b
    compare (TString a) (TString b) = compare a b
    compare (TChar a) (TChar b) = compare a b
    compare TT TT = EQ
    compare TNil TNil = EQ
    compare (TKeyword a) (TKeyword b) = compare a b
    compare (TList a) (TList b) = compare a b
    compare _ _ = error "can't compare terminals of different types"

boolToTerminal :: Bool -> Atom
boolToTerminal True  = TT
boolToTerminal False = TNil

terminalToBool :: Atom -> Bool
terminalToBool TT   = True
terminalToBool TNil = False
terminalToBool _    = error "T or Nil expected"

printTerminal :: Atom -> String
printTerminal (TInt int)         = show int
printTerminal (TFloat float)     = show float
printTerminal (TString string)   = string
printTerminal (TChar char)       = [char]
printTerminal TT                 = "T"
printTerminal TNil               = "Nil"
printTerminal (TKeyword keyword) = keyword
printTerminal (TList list)       = "(list " ++ handle_list list ++ ")"
    where handle_list [x]    = printTerminal x
          handle_list (x:xs) = printTerminal x ++ " " ++ handle_list xs
          handle_list []     = ""

printType :: Atom -> String
printType (TInt _)     = "Int"
printType (TFloat _)   = "Float"
printType (TString _)  = "String"
printType (TChar _)    = "Char"
printType TT           = "T"
printType TNil         = "Nil"
printType (TKeyword _) = "Keyword"
printType (TList _)    = "List"

isKeyword :: Atom -> Bool
isKeyword (TKeyword _) = True
isKeyword _            = False

fromKeyword :: Atom -> String
fromKeyword (TKeyword a) = a
fromKeyword _            = error "that's no keyword"

fromNumber :: Atom -> Float
fromNumber (TFloat f)     = f
fromNumber (TInt i)       = fromIntegral i
fromNumber _              = error "float or int expected"

fromFloat :: Atom -> Float
fromFloat (TFloat f) = f
fromFloat _          = error "float expected"

fromInt :: Atom -> Int
fromInt (TInt i) = i
fromInt _        = error "int expected"

analyze :: Tree String -> Tree Atom
analyze = fmap analyze_terminal
    where analyze_terminal "T" = TT
          analyze_terminal "Nil" = TNil
          analyze_terminal terminal
            | isJust tryInt    = TInt     $ fromJust tryInt
            | isJust tryFloat  = TFloat   $ fromJust tryFloat
            | isJust tryChar   = TChar    $ fromJust tryChar
            | isJust tryString = TString  $ fromJust tryString
            | otherwise        = TKeyword terminal
                where tryInt = readMaybe terminal :: Maybe Int
                      tryFloat = readMaybe terminal :: Maybe Float
                      tryChar = readMaybe terminal :: Maybe Char
                      tryString = readMaybe terminal :: Maybe String
