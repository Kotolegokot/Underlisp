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
            Quote         -> let (quote, rest) = handle_quote xs
                              in helper (quote : acc) rest
            --Backquote     -> let (backquote, rest) = handle_backqoute xs
            --                  in helper (backquote : acc) rest
            --Interpolation -> let (interpolation, rest) = handle_interpolation xs
            --                  in helper (interpolation : acc) rest
            --Unfold        -> let (unfold, rest) = handle_unfold xs
            --                  in helper (unfold : acc) rest
        helper acc []     = error "unexpected EOF in the middle of a list"

        handle_quote :: [Lexeme] -> (SExpr, [Lexeme])
        handle_quote (RightParen:_) = error "right paren after quote sign is forbidden"
        handle_quote (LeftParen:xs) = let (list, rest) = helper [] xs
                                       in (SList [SSymbol "quote", list], rest)
        handle_quote (Atom atom:xs) = (SList [SSymbol "quote", atom], xs)
        handle_quote (Quote:xs)     = let (quote, rest) = handle_quote xs
                                       in (SList [SSymbol "quote", quote], rest)
