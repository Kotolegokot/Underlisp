module Reader (read) where

import Prelude hiding (read)
import Base
import Lexer
import Parser
import Point

-- | first stage of any lisp interpreter
-- | takes a string and converts it into an s-expression
read :: Point -> String -> Eval [SExpr]
read point text = do
  lexemes <- tokenize point text
  parse lexemes
