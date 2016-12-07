module Parser (Lexem(..), parse) where

data Lexem = LeftParen | RightParen | Function String
    deriving (Eq, Show)

data State = None | InsideFunction

parse :: String -> [Lexem]
parse sequence = helper [] None sequence
    where helper list None xs@(x:rest) = case x of
                                      '(' -> helper (list ++ [LeftParen]) None rest
                                      ')' -> helper (list ++ [RightParen]) None rest
                                      ' ' -> helper list None rest
                                      _   -> helper list InsideFunction xs

          helper list InsideFunction sequence = helper2 [] sequence
            where helper2 function xs@(x:rest) = case x of
                                                   ' ' -> helper (list ++ [Function function]) None xs
                                                   '(' -> helper (list ++ [Function function]) None xs
                                                   ')' -> helper (list ++ [Function function]) None xs
                                                   _   -> helper2 (function ++ [x]) rest

                  helper2 function [] = helper2 function []

          helper list _ [] = list
