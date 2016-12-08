module Lexer (Lexeme(..), tokenize) where

data Lexeme = LeftParen | RightParen | Function String
    deriving (Eq, Show)

data State = None | String | OtherTerminal

-- | 'tokenize' takes a sequence of symbols and splits it into lexemes
tokenize :: String -> [Lexeme]
tokenize sequence = reverse $ helper [] None sequence
    where helper lexemes None xs@(x:rest) = case x of
                                              '(' -> helper (LeftParen : lexemes) None rest
                                              ')' -> helper (RightParen : lexemes) None rest
                                              ' ' -> helper lexemes None rest
                                              '"' -> helper lexemes String rest
                                              _   -> helper lexemes OtherTerminal xs

          helper lexemes String sequence = parse_string [] sequence
              where parse_string string xs@(x:rest)
                      | x == '"'  = helper (Function ('"' : string ++ "\"") : lexemes) None rest
                      | otherwise = parse_string (string ++ [x]) rest

                    parse_string string [] = error "unexpected EOF in the middle of a string"

                        

          helper lexemes OtherTerminal sequence = parse_terminal [] sequence
              where parse_terminal terminal xs@(x:rest)
                      | elem x " ()" = helper (Function terminal : lexemes) None xs
                      | otherwise    = parse_terminal (terminal ++ [x]) rest

                    parse_terminal terminal [] = helper (Function terminal : lexemes) None []

          helper lexemes _ [] = lexemes
