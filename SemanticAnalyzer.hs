module SemanticAnalyzer
    (Terminal(..),
    analyze,
    isKeyword,
    fromKeyword) where

import Text.Read
import Data.Maybe
import Data.Tree

data Terminal = TInteger Int | TFloat Float | TString String | TChar Char | TT | TNil | TKeyword String
  deriving (Eq, Show)

isKeyword (TKeyword _) = True
isKeyword _            = False

fromKeyword (TKeyword a) = a
fromKeyword _            = error "that's no keyword"

data State = None | SInteger | SFloat | SString | SChar

analyze :: Tree String -> Tree Terminal
analyze = fmap analyze_terminal
    where analyze_terminal "T" = TT
          analyze_terminal "Nil" = TNil
          analyze_terminal terminal
            | isJust tryInt    = TInteger $ fromJust tryInt
            | isJust tryFloat  = TFloat   $ fromJust tryFloat
            | isJust tryChar   = TChar    $ fromJust tryChar
            | isJust tryString = TString  $ fromJust tryString
            | otherwise        = TKeyword terminal
                where tryInt = readMaybe terminal :: Maybe Int
                      tryFloat = readMaybe terminal :: Maybe Float
                      tryChar = readMaybe terminal :: Maybe Char
                      tryString = readMaybe terminal :: Maybe String
