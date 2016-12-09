module Reader (read) where

import Prelude hiding (read)
import SExpr
import Lexer
import Parser

-- | first stage of any lisp interpreter
-- | takes a string and converts it into an s-expression
read :: String -> SExpr
read = parse . tokenize
