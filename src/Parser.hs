module Parser (parse) where

import Debug.Trace

import SExpr
import Expr
import Lexer
import Point
import Exception

-- | takes a list of lexemes and generates a complete s-expression
parse :: [(Lexeme, Point)] -> [SExpr]
parse [] = []
parse ((x,p):xs) = case x of
  Open    b -> let (sexpr, rest) = parse_list p b xs
               in  (replace_point sexpr p) : parse rest
  Closed  _ -> report p "redundant right bracket"
  Atom    a -> (SAtom p a)  : parse xs
  LString l -> str2list p l : parse xs
  Sugar   s -> let (sexpr, rest) = parse_sugar p s xs
               in  sexpr : parse rest

parse_list :: Point -> Char -> [(Lexeme, Point)] -> (SExpr, [(Lexeme, Point)])
parse_list p b pairs = case b of
  '(' -> parse_list' p '(' [] pairs
  '[' -> let (SList p' l, rest) = parse_list' p '[' [] pairs
         in  (SList p' (SAtom p' (ASymbol "bind") : l), rest)
  where parse_list' :: Point -> Char -> [SExpr] -> [(Lexeme, Point)] -> (SExpr, [(Lexeme, Point)])
        parse_list' p bracket acc ((x,p'):xs) = case x of
          Open    b -> let (subl, rest) = parse_list p' b xs
                       in  parse_list' p bracket (subl : acc) rest
          Closed  b -> if b == bracket
                       then (SList p $ reverse acc, xs)
                       else report p' $ "unmatching brackets: unclosed '" ++ [bracket] ++ "'"
          Atom    a -> parse_list' p bracket (SAtom p' a : acc) xs
          LString l -> parse_list' p bracket (str2list p' l : acc) xs
          Sugar   s -> let (expr, rest) = parse_sugar p' s xs
                       in  parse_list' p bracket (expr : acc) rest
        parse_list' p _       _   []      = report p "unexpected EOF in the middle of a list"


parse_sugar :: Point -> String -> [(Lexeme, Point)] -> (SExpr, [(Lexeme, Point)])
parse_sugar p s ((Closed  _,  p'):_)   = report p' $ "right paren after '" ++ s ++ "' is forbidden"
parse_sugar p s ((Open    b,  p'):xs)  = let (subl, rest) = parse_list p' b xs
                                         in (SList p [SAtom p (ASymbol s), subl], rest)
parse_sugar p s ((Atom    a,  p'):xs)  = (SList p' [symbol s, atom a], xs)
parse_sugar p s ((Sugar   s', p'):xs)  = let (subl, rest) = parse_sugar p' s' xs
                                         in (SList p [SAtom p (ASymbol s), subl], rest)
parse_sugar p s ((LString l,  p'):xs)  = (SList p [SAtom p (ASymbol s), str2list p l], xs)

str2list :: Point -> String -> SExpr
str2list p str = SList p (SAtom p (ASymbol "list") : chars)
  where chars = map (\(p,c) -> SAtom p (AChar c)) $ zip (iterate forward_column chars_p) str
        chars_p = forward_column p
