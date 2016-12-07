module Main where

import Data.Tree
import Lexer
import Parser
import SemanticAnalyzer
import Interpreter

main :: IO ()
main = do
    input <- getLine
    evaluate . interprete . analyze . parse . tokenize $ input
