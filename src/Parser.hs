module Parser (parse) where

import SExpr
import Lexer

-- ( ( ) atom ' ` ~ @)

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
            LeftParen     -> let (sublist, rest) = helper [] xs
                              in helper (sublist : acc) rest
            RightParen    -> (SList $ reverse acc, xs)
            Atom atom     -> helper (atom : acc) xs
            Sugar str     -> let (quote, rest) = handle_sugar xs str
                              in helper (quote : acc) rest
        helper acc []     = error "unexpected EOF in the middle of a list"

        handle_sugar :: [Lexeme] -> String -> (SExpr, [Lexeme])
        handle_sugar (RightParen:_)  str = error "right paren after quote sign is forbidden"
        handle_sugar (LeftParen:xs)  str = let (list, rest) = helper [] xs
                                            in (SList [SSymbol str, list], rest)
        handle_sugar (Atom atom:xs)  str = (SList [SSymbol str, atom], xs)
        handle_sugar (Sugar str2:xs) str = let (quote, rest) = handle_sugar xs str2
                                            in (SList [SSymbol str, quote], rest)

