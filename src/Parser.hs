module Parser (parse) where

import Base
import Lexer
import Point

-- | takes a list of lexemes and generates a complete s-expression
parse :: [(Lexeme, Point)] -> Lisp [SExpr]
parse [] = return []
parse ((x,p):xs) = case x of
  Open b -> do
    (sexpr, rest) <- parseList p b xs
    rest' <- parse rest
    return (setPoint sexpr p : rest')
  Closed  _ -> report p "redundant right bracket"
  LAtom a -> do
    xs' <- parse xs
    return (SAtom p a : xs')
  LString l -> do
    xs' <- parse xs
    return (strToList p l : xs')
  SugarCall s -> do
    (sexpr, rest) <- parseSugarCall p s xs
    rest' <- parse rest
    return (sexpr : rest')
  SugarApply s -> do
    (sexpr, rest) <- parseSugarApply p s xs
    rest' <- parse rest
    return (sexpr : rest')

parseList :: Point -> Char -> [(Lexeme, Point)] -> Lisp (SExpr, [(Lexeme, Point)])
parseList p b pairs = case b of
  '(' -> parseList' p '(' [] pairs
  '{' -> parseList' p '{' [] pairs
  '[' -> do
    (SList p' l, rest) <- parseList' p '[' [] pairs
    return  (SList p' (SAtom p' (ASymbol "bind") : l), rest)
  _   -> undefined
  where parseList' :: Point -> Char -> [SExpr] -> [(Lexeme, Point)] -> Lisp (SExpr, [(Lexeme, Point)])
        parseList' p bracket acc ((x,p'):xs) = case x of
          Open      b -> do
            (subl, rest) <- parseList p' b xs
            parseList' p bracket (subl : acc) rest
          Closed     b -> if b == bracket
                          then return (SList p $ reverse acc, xs)
                          else report p' $ "unmatching brackets: unclosed '" ++ [bracket] ++ "'"
          LAtom      a -> parseList' p bracket (SAtom p' a : acc) xs
          LString    l -> parseList' p bracket (strToList p' l : acc) xs
          SugarCall  s -> do
            (expr, rest) <- parseSugarCall p' s xs
            parseList' p bracket (expr : acc) rest
          SugarApply s -> do
            (expr, rest) <- parseSugarApply p' s xs
            parseList' p bracket (expr : acc) rest
        parseList' p _       _   []      = report p "unexpected EOF in the middle of a list"


parseSugarCall :: Point -> String -> [(Lexeme, Point)] -> Lisp (SExpr, [(Lexeme, Point)])
parseSugarCall p s ((Closed    _,  p'):_)   = report p' $ "right paren after '" ++ s ++ "' is forbidden"
parseSugarCall p s ((Open      b,  p'):xs)  = do
  (subl, rest) <- parseList p' b xs
  return (SList p [SAtom p (ASymbol s), subl], rest)
parseSugarCall p s ((LAtom     a,  p'):xs)  = return (SList p' [symbol s, atom a], xs)
parseSugarCall p s ((SugarCall s', p'):xs)  = do
  (subl, rest) <- parseSugarCall p' s' xs
  return (SList p [SAtom p (ASymbol s), subl], rest)
parseSugarCall p s ((SugarApply s', p'):xs) = do
  (subl, rest) <- parseSugarApply p' s' xs
  return (SList p [SAtom p (ASymbol s), subl] , rest)
parseSugarCall p s ((LString   l,  p'):xs)  = return (SList p [SAtom p (ASymbol s), strToList p l], xs)
parseSugarCall p s []                       = report p $ "unexpected EOF after '" ++ s ++ "'"

parseSugarApply :: Point -> String -> [(Lexeme, Point)] -> Lisp (SExpr, [(Lexeme, Point)])
parseSugarApply _ _ ((Closed _, p'):_)  = report p' "list expected"
parseSugarApply p s ((Open   b, p'):xs) = do
  (SList _ ls, rest) <- parseList p' b xs
  return (SList p' (SAtom p (ASymbol s):ls), rest)
parseSugarApply _ _ ((_, p'):_)         = report p' "list expected"
parseSugarApply p s []                  = report p $ "unexpected EOF after '" ++ s ++ "'"

strToList :: Point -> String -> SExpr
strToList p str = SList p (SAtom p (ASymbol "list") : chars)
  where chars = map (\(p,c) -> SAtom p (AChar c)) $ zip (tail $ iterate forwardColumn p) str
