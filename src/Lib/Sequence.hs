module Lib.Sequence (specialOperators) where

-- vector
import qualified Data.Vector as Vec
--import Data.Vector (Vector)

-- other
import Control.Monad (mapM, liftM, when, (<=<))
import Control.Conditional (cond)
import Data.IORef
import Safe (atMay)

-- local modules
import Base
import Evaluator

default (Int)

biIsEmpty :: IORef Scope -> [SExpr] -> EvalM SExpr
biIsEmpty _ [exp]
  | isList   exp = bool . null <$> getList exp
  | isVector exp = bool . Vec.null <$> getVector exp
  | isString exp = bool . null <$> getString exp
  | otherwise    = reportE (point exp) "sequence expected"
biIsEmpty _ _ = reportE' "just one argument requried"

biConcat :: IORef Scope -> [SExpr] -> EvalM SExpr
biConcat _ (sType:seqs) = toSequence sType =<< liftM concat (mapM getSequence seqs)
biConcat _ _            = reportE' "at least one argument expected"

toSequence :: SExpr -> [SExpr] -> EvalM SExpr
toSequence sType exps = do
  returnType <- getSymbol sType
  case returnType of
    "vector" -> return . vector $ Vec.fromList exps
    "list"   -> return $ list exps
    "string" -> listToString exps
    other    -> reportE (point sType) ("undefined type: '" ++ other ++ "'")

listToString :: [SExpr] -> EvalM SExpr
listToString = return . string <=< lts
  where lts (x:xs) = do
          char <- Base.getChar x
          rest <- lts xs
          return (char : rest)
        lts []     = return []

biNth :: IORef Scope -> [SExpr] -> EvalM SExpr
biNth _ [sN, seq] = do
  n <- getInt sN
  when (n < 0) $ reportE (point sN) "negative index"
  cond [(isList seq, case fromList seq `atMay` n of
            Nothing  -> reportE' $ "index too large: " ++ show n
            Just val -> return val)
       ,(isVector seq, case fromVector seq Vec.!? n of
            Nothing  -> reportE' $ "index too large: " ++ show n
            Just val -> return val)
       ,(isString seq, case fromString seq `atMay` n of
            Nothing  -> reportE' $ "index too large: " ++ show n
            Just val -> return $ char val)
       ,(otherwise, reportE (point seq) "sequence expected")]
biNth _ _         = reportE' "two arguments required"

biHead :: IORef Scope -> [SExpr] -> EvalM SExpr
biHead _ [expr]
  | isList   expr = if null lst
                    then reportE p "empty list"
                    else return $ head lst
  | isVector expr = if null vec
                    then reportE p "empty vector"
                    else return $ Vec.head vec
  | isString expr = if null str
                    then reportE p "empty string"
                    else return . char $ head str
  | otherwise     = reportE' "sequence expected"
  where lst = fromList expr
        vec = fromVector expr
        str = fromString expr
        p   = point expr
biHead _ _                   = reportE' "just one argument required"

biTail :: IORef Scope -> [SExpr] -> EvalM SExpr
biTail _ [expr]
  | isList   expr = if null lst
                    then reportE p "empty list"
                    else return . list $ tail lst
  | isVector expr = if null vec
                    then reportE p "empty vector"
                    else return . vector $ Vec.tail vec
  | isString expr = if null str
                    then reportE p "empty string"
                    else return . string $ tail str
  | otherwise     = reportE' "sequence expected"
  where lst = fromList expr
        vec = fromVector expr
        str = fromString expr
        p   = point expr
biTail _ _                  = reportE' "just one argument required"

specialOperators = [("empty?", Just 1, withEvaluatedArgs biIsEmpty)
                   ,("concat", Just 2, withEvaluatedArgs biConcat)
                   ,("nth",    Just 2, withEvaluatedArgs biNth)
                   ,("head",   Just 1,  withEvaluatedArgs biHead)
                   ,("tail",   Just 1,  withEvaluatedArgs biTail)]
