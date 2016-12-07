module Main where

import Lexer

main :: IO ()
main = do
    input <- getLine
    let lexems = tokenize input

    printLexemes lexems

printLexemes :: [Lexeme] -> IO ()
printLexemes (x:xs) = print x >> printLexemes xs
printLexemes [] = return ()
