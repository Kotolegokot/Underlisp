module Lexer (Lexeme (..)
             , scan) where

import Control.Monad.Except
import Control.Arrow ((>>>), (&&&))
import Data.Char (isSpace)
import Base
import Point

data Lexeme = Open Char | Closed Char | LAtom Atom | SugarCall String | SugarApply String
  deriving (Eq, Show)

data State = None | Comment | Char | String | Vector | OtherAtom
  deriving (Eq, Show)

-- | takes a string and splits it into lexems
scan :: Point -> String -> Except Fail [(Lexeme, Point)]
scan point = scan' point [] None
  where scan' point lexemes None xs@(x:rest)
          | isOpen x       = scan' (forward x point) ((Open x, point)                     : lexemes) None    rest
          | isClosed x     = scan' (forward x point) ((Closed (matchingBracket x), point) : lexemes) None    rest
          | x == '\''      = scan' (forward x point) ((SugarCall "quote",       point)    : lexemes) None    rest
          | x == '`'       = scan' (forward x point) ((SugarCall "backquote",   point)    : lexemes) None    rest
          | x == '~'       = scan' (forward x point) ((SugarCall "interpolate", point)    : lexemes) None    rest
          | x == '@'       = scan' (forward x point) ((SugarCall "unfold",      point)    : lexemes) None    rest
          | x == '#'       = scan' (forward x point) lexemes                                         Char    rest
          | x == '"'       = scan' (forward x point) lexemes                                         String  rest
          | x == ';'       = scan' (forward x point) lexemes                                         Comment rest
          | x == '!'       = scan' (forward x point) lexemes                                         Vector  rest
          | isSeparator x  = scan' (forward x point) lexemes                                         None    rest
          | otherwise      = scan' point             lexemes                                         OtherAtom xs
        scan' origPoint lexemes Vector seq@(x:xs)
          | isOpen x      = scan' origPoint ((SugarApply "vector", origPoint) : lexemes) None seq
          | otherwise     = parseAtom origPoint "!" seq
            where parseAtom point atom xs@(x:rest)
                    | isSeparator x = scan' point ((LAtom (strToAtom atom), origPoint) : lexemes) None xs
                    | otherwise     = parseAtom (forwardColumn point) (atom ++ [x]) rest
                  parseAtom point atom [] = scan' point ((LAtom (strToAtom atom), origPoint) : lexemes) None []
        scan' point lexemes Vector [] = scan' point ((LAtom (ASymbol "!"), point) : lexemes) None []
        scan' point lexemes Comment (x:xs) = case x of
          '\n' -> scan' (forward x point) lexemes None    xs
          _    -> scan' (forward x point) lexemes Comment xs
        scan' origPoint lexemes Char sequence = parse_char origPoint [] sequence
          where parse_char point name xs@(x:rest)
                  | isSeparator x = if null name
                                    then do c <- translateChar origPoint [x]
                                            scan' (forward x point) ((c, origPoint) : lexemes) None rest
                                    else do c <- translateChar origPoint name
                                            scan' point ((c, origPoint) : lexemes) None xs
                  | otherwise     = parse_char (forward x point) (name ++ [x]) rest
                parse_char point name [] = do
                  c <- translateChar origPoint name
                  scan' point ((c, origPoint) : lexemes) None []
        scan' origPoint lexemes String sequence = parse_string origPoint [] sequence
            where parse_string point string xs@(x:rest)
                    | x == '"'  = scan' (forward x point) ((LAtom $ AString (reverse string), origPoint) : lexemes) None rest
                    | otherwise = parse_string (forward x point) (x : string) rest
                  parse_string point string [] = reportR point "unexpected EOF in the middle of a string"
        scan' origPoint lexemes OtherAtom sequence = parse_atom origPoint [] sequence
            where parse_atom point atom xs@(x:rest)
                    | isSeparator x = scan' point ((LAtom (strToAtom atom), origPoint) : lexemes) None xs
                    | otherwise     = parse_atom (forwardColumn point) (atom ++ [x]) rest
                  parse_atom point atom [] = scan' point ((LAtom (strToAtom atom), origPoint) : lexemes) None []
        scan' _     lexemes _         []       = return $ reverse lexemes

matchingBracket :: Char -> Char
matchingBracket x = case x of
                       '(' -> ')'
                       ')' -> '('
                       '[' -> ']'
                       ']' -> '['
                       '{' -> '}'
                       '}' -> '{'
                       _   -> undefined

translateChar :: Point -> String -> Except Fail Lexeme
translateChar point name = case name of
  "space"   -> return . LAtom . AChar $ ' '
  "newline" -> return . LAtom . AChar $ '\n'
  "tab"     -> return . LAtom . AChar $ '\t'
  [c]       -> return . LAtom . AChar $ c
  other     -> reportR point $ "undefined character name: '" ++ other ++ "'"

isSeparator :: Char -> Bool
isSeparator a = isBracket a || isSpace a

isOpen :: Char -> Bool
isOpen = (`elem` "([{")

isClosed :: Char -> Bool
isClosed = (`elem` ")]}")

isBracket :: Char -> Bool
isBracket = isOpen &&& isClosed >>> uncurry (||)
