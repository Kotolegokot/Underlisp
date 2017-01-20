module Reader (read) where

import Prelude hiding (read)
import Base
import Point
import Lexer
import Parser

-- | first stage of any lisp interpreter
-- | takes a string and converts it into an s-expression
read :: Point -> String -> Lisp [SExpr]
read point text = do
  lexemes <- tokenize point text
  parse lexemes
