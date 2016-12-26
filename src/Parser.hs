module Parser (parse) where

import SExpr
import Expr
import Lexer

-- | takes a list of lexemes and generates a complete s-expression
parse :: [Lexeme] -> [SExpr]
parse [] = []
parse (x:xs) = case x of
  Open    b -> let (sexpr, rest) = parse_list b xs
               in  sexpr : parse rest
  Closed  _ -> error "redundant right bracket"
  Atom    a -> (atom a) : parse xs
  LString l -> str2list l : parse xs
  Sugar   s -> let (sexpr, rest) = parse_sugar s xs
               in  sexpr : parse rest

parse_list :: Char -> [Lexeme] -> (SExpr, [Lexeme])
parse_list b lexemes = case b of
  '(' -> parse_list' '(' [] lexemes
  '[' -> let (SList l, rest) = parse_list' '[' [] lexemes
         in  (list (symbol "bind" : l), rest)
  where parse_list' :: Char -> [SExpr] -> [Lexeme] -> (SExpr, [Lexeme])
        parse_list' bracket acc (x:xs) = case x of
          Open    b -> let (subl, rest) = parse_list b xs
                       in  parse_list' bracket (subl : acc) rest
          Closed  b -> if b == bracket
                       then (list $ reverse acc, xs)
                       else error $ "unmatching brackets: unclosed '" ++ [bracket] ++ "'"
          Atom    a -> parse_list' bracket (atom a : acc) xs
          LString l -> parse_list' bracket (str2list l : acc) xs
          Sugar   s -> let (expr, rest) = parse_sugar s xs
                       in  parse_list' bracket (expr : acc) rest
        parse_list' _       _    []    = error "unexpected EOF in the middle of a list"

parse_sugar :: String -> [Lexeme] -> (SExpr, [Lexeme])
parse_sugar s (Closed  _ :_)   = error $ "right paren after '" ++ s ++ "' is forbidden"
parse_sugar s (Open    b :xs)  = let (subl, rest) = parse_list b xs
                               in (list [symbol s, subl], rest)
parse_sugar s (Atom    a :xs)  = (list [symbol s, atom a], xs)
parse_sugar s (Sugar   s':xs)  = let (subl, rest) = parse_sugar s' xs
                               in (list [symbol s, subl], rest)
parse_sugar s (LString l :xs)  = (list [symbol s, str2list l], xs)

str2list :: String -> SExpr
str2list s = list (symbol "list" : map char s)
