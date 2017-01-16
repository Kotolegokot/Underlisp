module Lib.Sequence (builtinFunctions
                    ,specialOperators) where

import qualified Data.Vector as Vec
import Data.Vector (Vector)
import Safe (atMay)

import Base
import Fail
import Point

biIsEmpty :: [SExpr] -> Eval SExpr
biIsEmpty [expr]
  | isList expr   = return . bool . null $ fromList expr
  | isVector expr = return . bool . Vec.null $ fromVector expr
  | otherwise     = report (point expr) "sequence expected"
biIsEmpty _ = reportUndef "just one argument requried"

biConcat :: [SExpr] -> Eval SExpr
biConcat [sType, seq1, seq2]
  | not $ isSymbol sType  = report (point sType) "symbol expected"
  | not $ isSequence seq1 = report (point seq1) "sequence expected"
  | not $ isSequence seq2 = report (point seq2) "sequence expected"
  | otherwise             = toSequence (point sType) (fromSymbol sType) $ fromSequence seq1 ++ fromSequence seq2
biConcat _ = reportUndef "three arguments required"

toSequence :: Point -> String -> [SExpr] -> Eval SExpr
toSequence _ "vector" = return . vector . Vec.fromList
toSequence _ "list"   = return . list
toSequence p other    = const $ report p $ "undefined type: '" ++ other ++ "'"

biNth :: [SExpr] -> Eval SExpr
biNth [sN, seq]
  | not $ isInt sN = report (point sN) "int expected"
  | n < 0          = report (point sN) "negative index"
  | isList seq     = case fromList seq `atMay` n of
      Nothing  -> reportUndef $ "index too large: " ++ show n
      Just val -> return val
  | isVector seq   = case fromVector seq Vec.!? n of
      Nothing  -> reportUndef $ "index too large: " ++ show n
      Just val -> return val
  where n = fromInt sN
biNth _ = reportUndef "two arguments required"

builtinFunctions = [("empty?", Just (1 :: Int), biIsEmpty)
                   ,("concat", Just 2,          biConcat)
                   ,("nth",    Just 2,          biNth)]

specialOperators = []
