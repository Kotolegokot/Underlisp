module Parser (parse) where

import Debug.Trace
import SExpr
import Lexer
import Debug.Trace

-- | takes a list of lexemes and generates a complete s-expression
parse :: [Lexeme] -> [SExpr]
parse [] = []
parse (x:xs) = case x of
                 Open p -> let (sexpr, rest) = parse_list p xs
                           in sexpr : parse rest
                 _      -> error $ "open bracket expected at top level\n" ++
                                  "there are probably too many closed brackets"

parse_list :: Char -> [Lexeme] -> (SExpr, [Lexeme])
parse_list b lexemes = case b of
                         '(' -> parse_list' '(' [] lexemes
                         '[' -> let (SList list, rest) = parse_list' '[' [] lexemes
                                in (SList (SSymbol "bind" : list), rest)
  where parse_list' bracket acc (x:xs) =
          case x of
            Open b        -> let (sublist, rest) = parse_list b xs
                             in parse_list' bracket (sublist : acc) rest
            Closed b      -> if b == bracket
                               then (SList $ reverse acc, xs)
                               else error $ "unmatching brackets: unclosed '" ++ [bracket] ++ "'"
            Atom atom     -> parse_list' bracket (atom : acc) xs
            Sugar str     -> let (quote, rest) = handle_sugar xs str
                             in parse_list' bracket (quote : acc) rest
        parse_list' _       _   []     = error "unexpected EOF in the middle of a list"

        handle_sugar :: [Lexeme] -> String -> (SExpr, [Lexeme])
        handle_sugar (Closed _:_)    _   = error "right paren after quote sign is forbidden"
        handle_sugar (Open b:xs)     str = let (list, rest) = parse_list' b [] xs
                                           in (SList [SSymbol str, list], rest)
        handle_sugar (Atom atom:xs)  str = (SList [SSymbol str, atom], xs)
        handle_sugar (Sugar str2:xs) str = let (quote, rest) = handle_sugar xs str2
                                           in (SList [SSymbol str, quote], rest)

