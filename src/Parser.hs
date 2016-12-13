module Parser (parse) where

import SExpr
import Lexer

-- | takes a list of lexemes and generates a complete s-expression
parse :: [Lexeme] -> [SExpr]
parse [] = []
parse (x:xs)
  | x /= LeftParen = error "list of lexemes doesn't start with a left paren"
  | otherwise      = let (sexpr, rest) = parseList xs
                      in case rest of
                           [] -> [sexpr]
                           _  -> sexpr : parse rest

parseList :: [Lexeme] -> (SExpr, [Lexeme])
parseList lexemes = helper [] lexemes
  where helper acc (x:xs) =
          case x of
            Atom atom  -> helper (atom : acc) xs
            RightParen -> (SList $ reverse acc, xs)
            LeftParen  -> let (sublist, rest) = helper [] xs
                           in helper (sublist : acc) rest

        helper acc []     = error "unexpected EOF in the middle of a list"
