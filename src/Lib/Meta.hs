module Lib.Meta (builtinFunctions
                ,specialOperators) where

import Control.Monad.State
import qualified Reader
import Base
import Evaluator

default (Int)

soMacroExpand :: [SExpr] -> Lisp SExpr
soMacroExpand [exp] = do
  exp' <- eval exp
  expanded <- expandMacros [exp']
  return $ head expanded
soMacroExpand _       = reportE' "just one argument required"

soMacroExpand1 :: [SExpr] -> Lisp SExpr
soMacroExpand1 [exp] = eval exp >>= soMacroExpand1'
    where soMacroExpand1' l@(SList _ (SAtom _ (ASymbol sym):args)) = do
            result <- gets $ lookupMacro sym
            case result of
              Just m   -> callMacro m args
              Nothing  -> return l
          soMacroExpand1' other                                    = return other
soMacroExpand1 _       = reportE' "just one argument required"

-- | special operator quote
soQuote :: [SExpr] -> Lisp SExpr
soQuote [arg] = return arg
soQuote _     = reportE' "just one argument requried"

-- | special operator backquote
soBackquote :: [SExpr] -> Lisp SExpr
soBackquote [SList _ (SAtom p (ASymbol  "interpolate") : rest)]
  | length rest /= 1 = reportE p "just one argument required"
  | otherwise        = eval $ head rest
soBackquote [SList _ l] = list <$> mapM' (soBackquote . return) l
    where mapM' :: (SExpr -> Lisp SExpr) -> [SExpr] -> Lisp [SExpr]
          mapM' f []     = return []
          mapM' f (x:xs) = case x of
            SList _ [SAtom _ (ASymbol "unfold"), arg] -> do
              exps <- getList =<< eval arg
              rest <- mapM' f xs
              return $ exps ++ rest
            SList _ (SAtom p (ASymbol "unfold"):_)    -> reportE p "just one argument required"
            other                                     -> do
              result <- f other
              rest   <- mapM' f xs
              return $ result : rest
soBackquote [arg]        = return arg
soBackquote _            = reportE' "just one argument required"

-- | special operator interprete
soInterprete :: [SExpr] -> Lisp SExpr
soInterprete [arg] = do
  str <- getString =<< eval arg
  read <- forwardExcept $ Reader.read (point arg) str
  expandAndEvalScopeInterpolated read
soInterprete _     = reportE' "just one argument required"

soGensym :: [SExpr] -> Lisp SExpr
soGensym [] = do
  e <- get
  let (g, sym) = gensym (getG e) e
  modify $ setG g
  return sym
  where gensym :: Int -> Env -> (Int, SExpr)
        gensym n e = case envLookup ("G-" ++ show n) e of
          Just _  -> gensym (n + 1) e
          Nothing -> (n + 1, symbol $ "G-" ++ show n)
soGensym _  = reportE' "no arguments required"

-- | special operator eval
soEval :: [SExpr] -> Lisp SExpr
soEval args = do
  args' <- mapM eval args
  evalScopeInterpolated args'

-- | converts a string into a symbol
biToSymbol :: [SExpr] -> Lisp SExpr
biToSymbol [exp] = symbol <$> getString exp
biToSymbol _     = reportE' "just one argument required"

builtinFunctions = [("->symbol", Just 1, biToSymbol)]

specialOperators = [("macroexpand-1", Just 1, soMacroExpand1)
                   ,("macroexpand",   Just 1, soMacroExpand)
                   ,("quote",         Just 1, soQuote)
                   ,("backquote",     Just 1, soBackquote)
                   ,("interprete",    Just 1, soInterprete)
                   ,("gensym",        Just 0, soGensym)
                   ,("eval",          Just 1, soEval
                    )]
