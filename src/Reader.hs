module Reader (read) where

import Prelude hiding (read)
import Control.Monad.Except
import Base
import Point
import Lexer
import Parser

-- | First stage of any lisp interpreter.
-- Takes a string and convert it into an s-expression.
read :: Point -> String -> Except Fail [SExpr]
read point text = do
  lexemes <- scan point text
  parse lexemes
