module Lib.Sequence (builtinFunctions
                    ,specialOperators) where

import qualified Data.Vector as Vec
import Control.Monad (mapM, liftM, when)
import Control.Conditional (cond)
import Safe (atMay)
import Base

default (Int)

biIsEmpty :: [SExpr] -> Eval SExpr
biIsEmpty [exp]
  | isList exp   = bool . null <$> getList exp
  | isVector exp = bool . Vec.null <$> getVector exp
  | otherwise    = report (point exp) "sequence expected"
biIsEmpty _ = reportUndef "just one argument requried"

biConcat :: [SExpr] -> Eval SExpr
biConcat (sType:seqs) = toSequence sType =<< liftM concat (mapM getSequence seqs)
biConcat _            = reportUndef "at least one argument expected"

toSequence :: SExpr -> [SExpr] -> Eval SExpr
toSequence sType exps = do
  returnType <- getSymbol sType
  case returnType of
    "vector" -> return . vector $ Vec.fromList exps
    "list"   -> return $ list exps
    other    -> report (point sType) ("undefined type: '" ++ other ++ "'")

biNth :: [SExpr] -> Eval SExpr
biNth [sN, seq] = do
  n <- getInt sN
  when (n < 0) $ report (point sN) "negative index"
  cond [(isList seq, case fromList seq `atMay` n of
                       Nothing  -> reportUndef $ "index too large: " ++ show n
                       Just val -> return val)
       ,(isVector seq, case fromVector seq Vec.!? n of
                         Nothing  -> reportUndef $ "index too large: " ++ show n
                         Just val -> return val)
       ,(otherwise, report (point seq) "sequence expected")]
biNth _         = reportUndef "two arguments required"

builtinFunctions = [("empty?", Just 1, biIsEmpty)
                   ,("concat", Just 2, biConcat)
                   ,("nth",    Just 2, biNth)]

specialOperators = []
