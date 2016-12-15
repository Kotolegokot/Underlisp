module Lexer (Lexeme (..), tokenize) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char (isSpace)
import SExpr

data Lexeme = Open Char | Closed Char | Atom SExpr | Sugar String
  deriving (Eq, Show)

data State = None | Comment | String | OtherAtom
  deriving (Eq, Show)

-- | takes a string and splits it into lexems
tokenize :: String -> [Lexeme]
tokenize sequence = tokenize' [] None sequence
    where tokenize' lexemes None xs@(x:rest)
            | elem x "(["  = tokenize' (Open x                      : lexemes) None rest
            | elem x ")]"  = tokenize' (Closed (matching_bracket x) : lexemes) None rest
            | x == '\''    = tokenize' (Sugar "quote"               : lexemes) None rest
            | x == '`'     = tokenize' (Sugar "backquote"           : lexemes) None rest
            | x == '~'     = tokenize' (Sugar "interpolate"         : lexemes) None rest
            | x == '@'     = tokenize' (Sugar "unfold"              : lexemes) None rest
            | isSpace x    = tokenize' lexemes None rest
            | x == '"'     = tokenize' lexemes String rest
            | x == ';'     = tokenize' lexemes Comment rest
            | otherwise    = tokenize' lexemes OtherAtom xs

          tokenize' lexemes Comment (x:xs) = case x of
                                            '\n' -> tokenize' lexemes None xs
                                            _    -> tokenize' lexemes Comment xs

          tokenize' lexemes String sequence = parse_string [] sequence
              where parse_string string xs@(x:rest)
                      | x == '"'  = tokenize' (Atom (SString string) : lexemes) None rest
                      | otherwise = parse_string (string ++ [x]) rest

                    parse_string string [] = error "unexpected EOF in the middle of a string"

          tokenize' lexemes OtherAtom sequence = parse_atom [] sequence
              where parse_atom atom xs@(x:rest)
                      | isSpace x || elem x "[]()" = tokenize' (Atom (str2atom atom) : lexemes) None xs
                      | otherwise                  = parse_atom (atom ++ [x]) rest

                    parse_atom atom [] = tokenize' (Atom (str2atom atom) : lexemes) None []

          tokenize' lexemes _ [] = reverse lexemes

matching_bracket :: Char -> Char
matching_bracket x = case x of
                       '(' -> ')'
                       ')' -> '('
                       '[' -> ']'
                       ']' -> '['
                       '{' -> '}'
                       '}' -> '{'
                       _   -> undefined
