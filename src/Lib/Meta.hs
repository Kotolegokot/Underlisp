module Lib.Meta (builtinFunctions
                ,specialOperators) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef

-- local modules
import qualified Reader
import Base
import Evaluator
import Util

default (Int)

soMacroExpand :: IORef Scope -> [SExpr] -> EvalM SExpr
soMacroExpand scopeRef [exp] = do
  exp' <- evalAlone scopeRef exp
  expanded <- expandMacros scopeRef [exp']
  return $ head expanded
soMacroExpand _        _     = reportE' "just one argument required"

soMacroExpand1 :: IORef Scope -> [SExpr] -> EvalM SExpr
soMacroExpand1 scopeRef [exp] = evalAlone scopeRef exp >>= soMacroExpand1'
    where soMacroExpand1' l@(SList _ (SAtom _ (ASymbol sym):args)) = do
            result <- liftIO $ exploreIORefIO scopeRef (scLookup sym)
            case result of
              Just (SAtom p (AProcedure m@(Macro _ _ _ _))) -> call scopeRef p m args
              _                                             -> return l
          soMacroExpand1' other                                    = return other
soMacroExpand1 _        _     = reportE' "just one argument required"

-- | special operator quote
soQuote :: IORef Scope -> [SExpr] -> EvalM SExpr
soQuote _ [arg] = return arg
soQuote _ _     = reportE' "just one argument requried"

-- | special operator backquote
soBackquote :: IORef Scope -> [SExpr] -> EvalM SExpr
soBackquote scopeRef [SList _ (SAtom p (ASymbol  "interpolate") : rest)]
  | length rest /= 1 = reportE p "just one argument required"
  | otherwise        = evalAlone scopeRef $ head rest
soBackquote scopeRef [SList _ l] = list <$> mapM' (soBackquote scopeRef . return) l
    where mapM' :: (SExpr -> EvalM SExpr) -> [SExpr] -> EvalM [SExpr]
          mapM' f []     = return []
          mapM' f (x:xs) = case x of
            SList _ [SAtom _ (ASymbol "unfold"), arg] -> do
              exps <- getList =<< evalAlone scopeRef arg
              rest <- mapM' f xs
              return $ exps ++ rest
            SList _ (SAtom p (ASymbol "unfold"):_)    -> reportE p "just one argument required"
            other                                     -> do
              result <- f other
              rest   <- mapM' f xs
              return $ result : rest
soBackquote _ [arg]        = return arg
soBackquote _ _            = reportE' "just one argument required"

-- | Special operator interprete
soInterprete :: IORef Scope -> [SExpr] -> EvalM SExpr
soInterprete scopeRef [arg] = do
  str <- getString =<< evalAlone scopeRef arg
  read <- forwardExcept $ Reader.read (point arg) str
  expandEvalBody scopeRef read
soInterprete _        _     = reportE' "just one argument required"

soGensym :: IORef Scope -> [SExpr] -> EvalM SExpr
soGensym scopeRef [] = liftIO $ do
  (g, sym) <- gensym =<< exploreIORef scopeRef getG
  modifyIORef scopeRef (modifyG $ const g)
  return sym
  where gensym :: Int -> IO (Int, SExpr)
        gensym n = do
          result <- exploreIORefIO scopeRef (scLookup $ "G-" ++ show n)
          case result of
            Just _  -> gensym (n + 1)
            Nothing -> return (n + 1, symbol $ "G-" ++ show n)
soGensym _        _  = reportE' "no arguments required"

-- | Special operator eval
soEval :: IORef Scope -> [SExpr] -> EvalM SExpr
soEval scopeRef args = do
  args' <- evalAloneSeq scopeRef args
  evalBody scopeRef args'

-- | converts a string into a symbol
biToSymbol :: IORef Scope -> [SExpr] -> EvalM SExpr
biToSymbol _ [exp] = symbol <$> getString exp
biToSymbol _ _     = reportE' "just one argument required"

builtinFunctions = [("->symbol", Just 1, biToSymbol)]

specialOperators = [("macroexpand-1", Just 1, soMacroExpand1)
                   ,("macroexpand",   Just 1, soMacroExpand)
                   ,("quote",         Just 1, soQuote)
                   ,("backquote",     Just 1, soBackquote)
                   ,("interprete",    Just 1, soInterprete)
                   ,("gensym",        Just 0, soGensym)
                   ,("eval",          Just 1, soEval)]
