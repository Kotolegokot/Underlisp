module Lexer (Lexeme (..)
             , tokenize) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (isSpace)
import Atom
import SExpr
import LexicalEnvironment
import Util
import Point

data Lexeme = Open Char | Closed Char | Atom (Atom LEnv SExpr) | LString String | Sugar String
  deriving Eq

data State = None | Comment | Char | String | OtherAtom
  deriving (Eq, Show)

-- | takes a string and splits it into lexems
tokenize :: Point -> String -> [(Lexeme, Point)]
tokenize point sequence = tokenize' point [] None sequence
    where tokenize' point lexemes None xs@(x:rest)
            | is_open x      = tokenize' (forward x point) ((Open x, point)                      : lexemes) None    rest
            | is_closed x    = tokenize' (forward x point) ((Closed (matching_bracket x), point) : lexemes) None    rest
            | x == '\''      = tokenize' (forward x point) ((Sugar "quote",       point)         : lexemes) None    rest
            | x == '`'       = tokenize' (forward x point) ((Sugar "backquote",   point)         : lexemes) None    rest
            | x == '~'       = tokenize' (forward x point) ((Sugar "interpolate", point)         : lexemes) None    rest
            | x == '@'       = tokenize' (forward x point) ((Sugar "unfold",      point)         : lexemes) None    rest
            | x == '#'       = tokenize' (forward x point) lexemes                                          Char    rest
            | x == '"'       = tokenize' (forward x point) lexemes                                          String  rest
            | x == ';'       = tokenize' (forward x point) lexemes                                          Comment rest
            | is_separator x = tokenize' (forward x point) lexemes                                          None    rest
            | otherwise      = tokenize' (forward x point) lexemes                                          OtherAtom xs

          tokenize' point lexemes Comment (x:xs) = case x of
            '\n' -> tokenize' (forward x point) lexemes None    xs
            _    -> tokenize' (forward x point) lexemes Comment xs

          tokenize' point lexemes Char sequence = parse_char point [] sequence
            where parse_char point name xs@(x:rest)
                    | is_separator x = tokenize' (forward x point) ((translate_char point name, point) : lexemes) None xs
                    | otherwise      = parse_char (forward x point) (name ++ [x]) rest
                  parse_char point name [] = tokenize' point ((translate_char point name, point) : lexemes) None []

          tokenize' point lexemes String sequence = parse_string point [] sequence
              where parse_string point string xs@(x:rest)
                      | x == '"'  = tokenize' (forward x point) ((LString (reverse string), point) : lexemes) None rest
                      | otherwise = parse_string (forward x point) (x : string) rest
                    parse_string point string [] = report point "unexpected EOF in the middle of a string"

          tokenize' point lexemes OtherAtom sequence = parse_atom point [] sequence
              where parse_atom point atom xs@(x:rest)
                      | is_separator x = tokenize' (forward x point) ((Atom (str2atom atom), point) : lexemes) None xs
                      | otherwise      = parse_atom (forward_column point) (atom ++ [x]) rest
                    parse_atom point atom [] = tokenize' point ((Atom (str2atom atom), point) : lexemes) None []

          tokenize' _     lexemes _         []       = reverse lexemes

matching_bracket :: Char -> Char
matching_bracket x = case x of
                       '(' -> ')'
                       ')' -> '('
                       '[' -> ']'
                       ']' -> '['
                       '{' -> '}'
                       '}' -> '{'
                       _   -> undefined

translate_char :: Point -> String -> Lexeme
translate_char point name = Atom . AChar $ case name of
  "space"   -> ' '
  "newline" -> '\n'
  "tab"     -> '\t'
  [c]       -> c
  other     -> report point $ "undefined character name: '" ++ other ++ "'"

is_separator :: Char -> Bool
is_separator a = (is_bracket a) || isSpace a

is_open :: Char -> Bool
is_open = (`elem` "([{")

is_closed :: Char -> Bool
is_closed = (`elem` ")]}")

is_bracket :: Char -> Bool
is_bracket a = is_open a || is_closed a
