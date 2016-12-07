module Lexer (Lexeme(..), tokenize) where

data Lexeme = LeftParen | RightParen | Function String
    deriving (Eq, Show)

data State = None | Terminal

-- | 'tokenize' takes a sequence of symbols and splits it into lexemes
tokenize :: String -> [Lexeme]
tokenize sequence = reverse $ helper [] None sequence
    where helper lexemes None xs@(x:rest) = case x of
                                              '(' -> helper (LeftParen : lexemes) None rest
                                              ')' -> helper (RightParen : lexemes) None rest
                                              ' ' -> helper lexemes None rest
                                              _   -> helper lexemes Terminal xs

          helper lexemes Terminal sequence = parse_terminal [] sequence
              where parse_terminal terminal xs@(x:rest)
                      | elem x " ()" = helper (Function terminal : lexemes) None xs
                      | otherwise    = parse_terminal (terminal ++ [x]) rest

                    parse_terminal terminal [] = helper (Function terminal : lexemes) None []

          helper lexemes _ [] = lexemes
