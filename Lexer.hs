module Lexer (Lexeme (..), tokenize) where

import Data.Char (isSpace)
import SExpr

-- | a lexeme is either a paren or a single atom
data Lexeme = LeftParen | RightParen | Atom SExpr
    deriving (Eq, Show)

data State = None | String | OtherAtom

-- | takes a string and splits it into lexems
tokenize :: String -> [Lexeme]
tokenize sequence = helper [] None sequence
    where helper lexemes None xs@(x:rest)
            | x == '('  = helper (LeftParen : lexemes) None rest
            | x == ')'  = helper (RightParen : lexemes) None rest
            | isSpace x = helper lexemes None rest
            | x == '"'  = helper lexemes String rest
            | otherwise = helper lexemes OtherAtom xs

          helper lexemes String sequence = parse_string [] sequence
              where parse_string string xs@(x:rest)
                      | x == '"'  = helper (Atom (SString string) : lexemes) None rest
                      | otherwise = parse_string (string ++ [x]) rest

                    parse_string string [] = error "unexpected EOF in the middle of a string"

          helper lexemes OtherAtom sequence = parse_terminal [] sequence
              where parse_terminal atom xs@(x:rest)
                      | isSpace x || elem x "()" = helper (Atom (str2atom atom) : lexemes) None xs
                      | otherwise                = parse_terminal (atom ++ [x]) rest

                    parse_terminal atom [] = helper (Atom (str2atom atom) : lexemes) None []

          helper lexemes _ [] = reverse lexemes
