module Lexer (Lexeme (..)
             , tokenize) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (isSpace)
import Atom
import SExpr
import LexicalEnvironment
import Util

data Lexeme = Open Char | Closed Char | Atom (Atom LEnv SExpr) | LString String | Sugar String
  deriving Eq

data State = None | Comment | String | OtherAtom
  deriving (Eq, Show)

-- | takes a string and splits it into lexems
tokenize :: Point -> String -> [(Lexeme, Point)]
tokenize point sequence = tokenize' point [] None sequence
    where tokenize' point lexemes None xs@(x:rest)
            | elem x "(["  = tokenize' (forward_column point) ((Open x, point)                      : lexemes) None    rest
            | elem x ")]"  = tokenize' (forward_column point) ((Closed (matching_bracket x), point) : lexemes) None    rest
            | x == '\''    = tokenize' (forward_column point) ((Sugar "quote", point)               : lexemes) None    rest
            | x == '`'     = tokenize' (forward_column point) ((Sugar "backquote", point)           : lexemes) None    rest
            | x == '~'     = tokenize' (forward_column point) ((Sugar "interpolate", point)         : lexemes) None    rest
            | x == '@'     = tokenize' (forward_column point) ((Sugar "unfold", point)              : lexemes) None    rest
            | x == '\n'    = tokenize' (forward_row    point) lexemes                                          None    rest
            | isSpace x    = tokenize' (forward_column point) lexemes                                          None    rest
            | x == '"'     = tokenize' (forward_column point) lexemes                                          String  rest
            | x == ';'     = tokenize' (forward_column point) lexemes                                          Comment rest
            | otherwise    = tokenize' (forward_column point) lexemes                                          OtherAtom xs

          tokenize' point lexemes Comment (x:xs) = case x of
            '\n' -> tokenize' (forward_row    point) lexemes None    xs
            _    -> tokenize' (forward_column point) lexemes Comment xs

          tokenize' point lexemes String sequence = parse_string point [] sequence
              where parse_string point string xs@(x:rest)
                      | x == '"'  = tokenize' (forward_column point) ((LString (reverse string), point) : lexemes) None rest
                      | x == '\n' = parse_string (forward_row    point) (x : string) rest
                      | otherwise = parse_string (forward_column point) (x : string) rest

                    parse_string point string [] = shit point "unexpected EOF in the middle of a string"

          tokenize' point lexemes OtherAtom sequence = parse_atom point [] sequence
              where parse_atom point atom xs@(x:rest)
                      | x == '\n'                  = tokenize' (forward_row    point) ((Atom (str2atom atom), point) : lexemes) None xs
                      | isSpace x || elem x "[]()" = tokenize' (forward_column point) ((Atom (str2atom atom), point) : lexemes) None xs
                      | otherwise                  = parse_atom (forward_column point) (atom ++ [x]) rest

                    parse_atom point atom []       = tokenize' point ((Atom (str2atom atom), point) : lexemes) None []

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
