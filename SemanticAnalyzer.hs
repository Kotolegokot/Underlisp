module SemanticAnalyzer
    (Terminal(..),
    analyze,
    isKeyword,
    fromKeyword,
    fromNumber,
    fromFloat,
    fromInt,
    printTerminal,
    printType) where

import Text.Read
import Data.Maybe
import Data.Tree

data Terminal = TInt Int | TFloat Float | TString String | TChar Char | TT | TNil | TKeyword String
  deriving (Eq, Show)

printTerminal :: Terminal -> String
printTerminal (TInt int)         = show int
printTerminal (TFloat float)     = show float
printTerminal (TString string)   = string
printTerminal (TChar char)       = [char]
printTerminal TT                 = "T"
printTerminal TNil               = "TNil"
printTerminal (TKeyword keyword) = keyword

printType :: Terminal -> String
printType (TInt _)     = "Int"
printType (TFloat _)   = "Float"
printType (TString _)  = "String"
printType (TChar _)    = "Char"
printType TT           = "T"
printType TNil         = "Nil"
printType (TKeyword _) = "Keyword"

isKeyword :: Terminal -> Bool
isKeyword (TKeyword _) = True
isKeyword _            = False

fromKeyword :: Terminal -> String
fromKeyword (TKeyword a) = a
fromKeyword _            = error "that's no keyword"

fromNumber :: Terminal -> Float
fromNumber (TFloat f)     = f
fromNumber (TInt i)       = fromIntegral i
fromNumber _              = error "float or int expected"

fromFloat :: Terminal -> Float
fromFloat (TFloat f) = f
fromFloat _          = error "float expected"

fromInt :: Terminal -> Int
fromInt (TInt i) = i
fromInt _        = error "int expected"

analyze :: Tree String -> Tree Terminal
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
