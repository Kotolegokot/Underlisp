module Parser (parse) where

import Debug.Trace

import Base
import Lexer
import Point
import Fail

-- | takes a list of lexemes and generates a complete s-expression
parse :: [(Lexeme, Point)] -> Eval [SExpr]
parse [] = return []
parse ((x,p):xs) = case x of
  Open    b -> do
    (sexpr, rest) <- parseList p b xs
    rest' <- parse rest
    return (setPoint sexpr p : rest')
  Closed  _ -> report p "redundant right bracket"
  LAtom   a -> do
    xs' <- parse xs
    return (SAtom p a : xs')
  LString l -> do
    xs' <- parse xs
    return (strToList p l : xs')
  Sugar   s -> do
    (sexpr, rest) <- parseSugar p s xs
    rest' <- parse rest
    return (sexpr : rest')

parseList :: Point -> Char -> [(Lexeme, Point)] -> Eval (SExpr, [(Lexeme, Point)])
parseList p b pairs = case b of
  '(' -> parseList' p '(' [] pairs
  '{' -> parseList' p '{' [] pairs
  '[' -> do
    (SList p' l, rest) <- parseList' p '[' [] pairs
    return  (SList p' (SAtom p' (ASymbol "bind") : l), rest)
  where parseList' :: Point -> Char -> [SExpr] -> [(Lexeme, Point)] -> Eval (SExpr, [(Lexeme, Point)])
        parseList' p bracket acc ((x,p'):xs) = case x of
          Open    b -> do
            (subl, rest) <- parseList p' b xs
            parseList' p bracket (subl : acc) rest
          Closed  b -> if b == bracket
                       then return (SList p $ reverse acc, xs)
                       else report p' $ "unmatching brackets: unclosed '" ++ [bracket] ++ "'"
          LAtom   a -> parseList' p bracket (SAtom p' a : acc) xs
          LString l -> parseList' p bracket (strToList p' l : acc) xs
          Sugar   s -> do
            (expr, rest) <- parseSugar p' s xs
            parseList' p bracket (expr : acc) rest
        parseList' p _       _   []      = report p "unexpected EOF in the middle of a list"


parseSugar :: Point -> String -> [(Lexeme, Point)] -> Eval (SExpr, [(Lexeme, Point)])
parseSugar p s ((Closed  _,  p'):_)   = report p' $ "right paren after '" ++ s ++ "' is forbidden"
parseSugar p s ((Open    b,  p'):xs)  = do
  (subl, rest) <- parseList p' b xs
  return (SList p [SAtom p (ASymbol s), subl], rest)
parseSugar p s ((LAtom   a,  p'):xs)  = return (SList p' [symbol s, atom a], xs)
parseSugar p s ((Sugar   s', p'):xs)  = do
  (subl, rest) <- parseSugar p' s' xs
  return (SList p [SAtom p (ASymbol s), subl], rest)
parseSugar p s ((LString l,  p'):xs)  = return (SList p [SAtom p (ASymbol s), strToList p l], xs)

strToList :: Point -> String -> SExpr
strToList p str = SList p (SAtom p (ASymbol "list") : chars)
  where chars = map (\(p,c) -> SAtom p (AChar c)) $ zip (tail $ iterate forwardColumn p) str
