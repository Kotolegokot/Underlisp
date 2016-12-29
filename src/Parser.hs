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
  Open    b -> let (sexpr, rest) = parseList p b xs
               in  (replacePoint sexpr p) : parse rest
  Closed  _ -> report p "redundant right bracket"
  Atom    a -> (SAtom p a)  : parse xs
  LString l -> strToList p l : parse xs
  Sugar   s -> let (sexpr, rest) = parseSugar p s xs
               in  sexpr : parse rest

parseList :: Point -> Char -> [(Lexeme, Point)] -> (SExpr, [(Lexeme, Point)])
parseList p b pairs = case b of
  '(' -> parseList' p '(' [] pairs
  '[' -> let (SList p' l, rest) = parseList' p '[' [] pairs
         in  (SList p' (SAtom p' (ASymbol "bind") : l), rest)
  where parseList' :: Point -> Char -> [SExpr] -> [(Lexeme, Point)] -> (SExpr, [(Lexeme, Point)])
        parseList' p bracket acc ((x,p'):xs) = case x of
          Open    b -> let (subl, rest) = parseList p' b xs
                       in  parseList' p bracket (subl : acc) rest
          Closed  b -> if b == bracket
                       then (SList p $ reverse acc, xs)
                       else report p' $ "unmatching brackets: unclosed '" ++ [bracket] ++ "'"
          Atom    a -> parseList' p bracket (SAtom p' a : acc) xs
          LString l -> parseList' p bracket (strToList p' l : acc) xs
          Sugar   s -> let (expr, rest) = parseSugar p' s xs
                       in  parseList' p bracket (expr : acc) rest
        parseList' p _       _   []      = report p "unexpected EOF in the middle of a list"


parseSugar :: Point -> String -> [(Lexeme, Point)] -> (SExpr, [(Lexeme, Point)])
parseSugar p s ((Closed  _,  p'):_)   = report p' $ "right paren after '" ++ s ++ "' is forbidden"
parseSugar p s ((Open    b,  p'):xs)  = let (subl, rest) = parseList p' b xs
                                        in (SList p [SAtom p (ASymbol s), subl], rest)
parse_sugar p s ((Atom    a,  p'):xs)  = (SList p' [symbol s, atom a], xs)
parse_sugar p s ((Sugar   s', p'):xs)  = let (subl, rest) = parseSugar p' s' xs
                                         in (SList p [SAtom p (ASymbol s), subl], rest)
parse_sugar p s ((LString l,  p'):xs)  = (SList p [SAtom p (ASymbol s), strToList p l], xs)

strToList :: Point -> String -> SExpr
strToList p str = SList p (SAtom p (ASymbol "list") : chars)
  where chars = map (\(p,c) -> SAtom p (AChar c)) $ zip (tail $ iterate forwardColumn p) str
