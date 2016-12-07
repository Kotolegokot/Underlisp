module Main where

import Data.Tree
import Lexer
import Parser

main :: IO ()
main = do
    input <- getLine
    let lexemes = tokenize input
    putStr $ drawTree $ parse $ tokenize input
    --mapM_ print lexemes
