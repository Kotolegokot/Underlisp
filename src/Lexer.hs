module Lexer (Lexeme (..)
             , tokenize) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (isSpace)
import Base
import Point
import Exception

data Lexeme = Open Char | Closed Char | LAtom Atom | LString String | Sugar String
  deriving Eq

data State = None | Comment | Char | String | OtherAtom
  deriving (Eq, Show)

-- | takes a string and splits it into lexems
tokenize :: Point -> String -> Eval [(Lexeme, Point)]
tokenize point sequence = tokenize' point [] None sequence
    where tokenize' point lexemes None xs@(x:rest)
            | isOpen x       = tokenize' (forward x point) ((Open x, point)                      : lexemes) None    rest
            | isClosed x     = tokenize' (forward x point) ((Closed (matchingBracket x), point)  : lexemes) None    rest
            | x == '\''      = tokenize' (forward x point) ((Sugar "quote",       point)         : lexemes) None    rest
            | x == '`'       = tokenize' (forward x point) ((Sugar "backquote",   point)         : lexemes) None    rest
            | x == '~'       = tokenize' (forward x point) ((Sugar "interpolate", point)         : lexemes) None    rest
            | x == '@'       = tokenize' (forward x point) ((Sugar "unfold",      point)         : lexemes) None    rest
            | x == '#'       = tokenize' (forward x point) lexemes                                          Char    rest
            | x == '"'       = tokenize' (forward x point) lexemes                                          String  rest
            | x == ';'       = tokenize' (forward x point) lexemes                                          Comment rest
            | isSeparator x  = tokenize' (forward x point) lexemes                                          None    rest
            | otherwise      = tokenize' point             lexemes                                          OtherAtom xs

          tokenize' point lexemes Comment (x:xs) = case x of
            '\n' -> tokenize' (forward x point) lexemes None    xs
            _    -> tokenize' (forward x point) lexemes Comment xs

          tokenize' origPoint lexemes Char sequence = parse_char origPoint [] sequence
            where parse_char point name xs@(x:rest)
                    | isSeparator x = do
                        c <- translateChar origPoint name
                        tokenize' point ((c, origPoint) : lexemes) None xs
                    | otherwise     = parse_char (forward x point) (name ++ [x]) rest
                  parse_char point name [] = do
                    c <- translateChar origPoint name
                    tokenize' point ((c, origPoint) : lexemes) None []

          tokenize' origPoint lexemes String sequence = parse_string origPoint [] sequence
              where parse_string point string xs@(x:rest)
                      | x == '"'  = tokenize' (forward x point) ((LString (reverse string), origPoint) : lexemes) None rest
                      | otherwise = parse_string (forward x point) (x : string) rest
                    parse_string point string [] = report point "unexpected EOF in the middle of a string"

          tokenize' origPoint lexemes OtherAtom sequence = parse_atom origPoint [] sequence
              where parse_atom point atom xs@(x:rest)
                      | isSeparator x = tokenize' point ((LAtom (strToAtom atom), origPoint) : lexemes) None xs
                      | otherwise      = parse_atom (forwardColumn point) (atom ++ [x]) rest
                    parse_atom point atom [] = tokenize' point ((LAtom (strToAtom atom), origPoint) : lexemes) None []

          tokenize' _     lexemes _         []       = return $ reverse lexemes

matchingBracket :: Char -> Char
matchingBracket x = case x of
                       '(' -> ')'
                       ')' -> '('
                       '[' -> ']'
                       ']' -> '['
                       '{' -> '}'
                       '}' -> '{'
                       _   -> undefined

translateChar :: Point -> String -> Eval Lexeme
translateChar point name = case name of
  "space"   -> return . LAtom . AChar $ ' '
  "newline" -> return . LAtom . AChar $ '\n'
  "tab"     -> return . LAtom . AChar $ '\t'
  [c]       -> return . LAtom . AChar $ c
  other     -> report point $ "undefined character name: '" ++ other ++ "'"

isSeparator :: Char -> Bool
isSeparator a = (isBracket a) || isSpace a

isOpen :: Char -> Bool
isOpen = (`elem` "([{")

isClosed :: Char -> Bool
isClosed = (`elem` ")]}")

isBracket :: Char -> Bool
isBracket a = isOpen a || isClosed a
