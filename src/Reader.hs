module Reader (read) where

import Prelude hiding (read)
import SExpr
import Lexer
import Parser
import Util
import Point

-- | first stage of any lisp interpreter
-- | takes a string and converts it into an s-expression
read :: Point -> String -> [SExpr]
read point = parse . fmap fst . (tokenize point)
