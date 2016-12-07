module Main where

import Parser

main :: IO ()
main = do
    input <- getLine
    let lexems = parse input

    printLexems lexems

printLexems :: [Lexem] -> IO ()
printLexems (x:xs) = print x >> printLexems xs
printLexems [] = return ()
