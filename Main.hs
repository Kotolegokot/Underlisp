module Main where

import Data.Tree
import Lexer
import Parser
import SemanticAnalyzer

main :: IO ()
main = do
    input <- getLine
    let lexemes = tokenize input

    putStr $ drawTree $ fmap show $ analyze $ parse $ tokenize input
    --mapM_ print lexemes
