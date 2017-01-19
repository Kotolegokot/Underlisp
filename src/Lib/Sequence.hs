module Lib.Sequence (builtinFunctions
                    ,specialOperators) where

import qualified Data.Vector as Vec
import Control.Monad (mapM, liftM)
import Safe (atMay)

import Base

biIsEmpty :: [SExpr] -> Eval SExpr
biIsEmpty [expr]
  | isList expr   = return . bool . null $ fromList expr
  | isVector expr = return . bool . Vec.null $ fromVector expr
  | otherwise     = report (point expr) "sequence expected"
biIsEmpty _ = reportUndef "just one argument requried"

biConcat :: [SExpr] -> Eval SExpr
biConcat (sType:seqs) = toSequence sType =<< liftM concat (mapM getSequence seqs)
biConcat _            = reportUndef "at least one argument expected"

getSequence :: SExpr -> Eval [SExpr]
getSequence expr
  | not $ isSequence expr = report (point expr) "sequence expected"
  | otherwise             = return $ fromSequence expr

toSequence :: SExpr -> [SExpr] -> Eval SExpr
toSequence sType
  | not $ isSymbol sType   = const $ report p "symbol expected"
  | otherwise              = case returnType of
      "vector" -> return . vector . Vec.fromList
      "list"   -> return . list
      other    -> const $ report p ("undefined type: '" ++ other ++ "'")
    where returnType = fromSymbol sType
          p          = point sType

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
