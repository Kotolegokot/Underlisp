module Lib.Sequence (builtinFunctions
                    ,specialOperators) where

import qualified Data.Vector as Vec
import Control.Monad (mapM, liftM, when)
import Control.Conditional (cond)
import Safe (atMay)
import Base

default (Int)

biIsEmpty :: [SExpr] -> Lisp SExpr
biIsEmpty [exp]
  | isList exp   = bool . null <$> getList exp
  | isVector exp = bool . Vec.null <$> getVector exp
  | otherwise    = reportE (point exp) "sequence expected"
biIsEmpty _ = reportE' "just one argument requried"

biConcat :: [SExpr] -> Lisp SExpr
biConcat (sType:seqs) = toSequence sType =<< liftM concat (mapM getSequence seqs)
biConcat _            = reportE' "at least one argument expected"

toSequence :: SExpr -> [SExpr] -> Lisp SExpr
toSequence sType exps = do
  returnType <- getSymbol sType
  case returnType of
    "vector" -> return . vector $ Vec.fromList exps
    "list"   -> return $ list exps
    other    -> reportE (point sType) ("undefined type: '" ++ other ++ "'")

biNth :: [SExpr] -> Lisp SExpr
biNth [sN, seq] = do
  n <- getInt sN
  when (n < 0) $ reportE (point sN) "negative index"
  cond [(isList seq, case fromList seq `atMay` n of
                       Nothing  -> reportE' $ "index too large: " ++ show n
                       Just val -> return val)
       ,(isVector seq, case fromVector seq Vec.!? n of
                         Nothing  -> reportE' $ "index too large: " ++ show n
                         Just val -> return val)
       ,(otherwise, reportE (point seq) "sequence expected")]
biNth _         = reportE' "two arguments required"

builtinFunctions = [("empty?", Just 1, biIsEmpty)
                   ,("concat", Just 2, biConcat)
                   ,("nth",    Just 2, biNth)]

specialOperators = []
