module Parser (parse) where

import Data.Tree
import Lexer

parse :: [Lexeme] -> Tree String
parse [] = error "empty file"
parse (LeftParen:rest) = let (tree, rest2) = parseList rest
                          in if not $ null rest2 then error "trailing characters after first list" else tree
                         where parseList (x:rest) =
                                 case x of 
                                   Atom     str -> let (args, rest2) = parseArgs rest [] 
                                                    in (Node str args, rest2) 
                                   RightParen   -> error "empty list" 
                                   LeftParen    -> error "a list at the beginning of another list" 

                               parseList [] = error "unexpected EOF" 

                               parseArgs (x:rest) args =
                                   case x of
                                     Atom     str -> parseArgs rest (args ++ [Node str []])
                                     RightParen   -> (args, rest)
                                     LeftParen    -> let (tree, rest2) = parseList rest
                                                      in parseArgs rest2 (args ++ [tree])
                               parseArgs [] _ = error "unexpected EOF"

parse _ = error "a left paren expected at the beginning of file"
