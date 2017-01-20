module Lib.Meta (builtinFunctions
                ,specialOperators) where

import Control.Monad ((>=>))
import qualified Reader
import Base
import Evaluator

default (Int)

soMacroExpand :: Env -> [SExpr] -> Lisp (Env, SExpr)
soMacroExpand e [sexpr] = do
  (_, sexpr') <- eval e sexpr
  expanded <- expandMacros e [sexpr']
  return (e, head expanded)
soMacroExpand _ _       = reportUndef "just one argument required"

soMacroExpand1 :: Env -> [SExpr] -> Lisp (Env, SExpr)
soMacroExpand1 e [sexpr] = do
  (_, evaluated) <- eval e sexpr
  soMacroExpand1' evaluated
    where soMacroExpand1' l@(SList _ (SAtom _ (ASymbol sym):args)) = case lookupMacro sym e of
            Just m   -> do
              result <- callMacro e m args
              return (e, result)
            Nothing  -> return (e, l)
          soMacroExpand1' other                               = return (e, other)
soMacroExpand1 _ _       = reportUndef "just one argument required"

-- | special operator quote
soQuote :: Env -> [SExpr] -> Lisp (Env, SExpr)
soQuote e [arg] = return (e, arg)
soQuote _ _     = reportUndef "just one argument requried"

-- | special operator backquote
soBackquote :: Env -> [SExpr] -> Lisp (Env, SExpr)
soBackquote e [SList _ (SAtom p (ASymbol  "interpolate") : rest)]
  | length rest /= 1 = report p "just one argument required"
  | otherwise        = do
      (_, expr) <- eval e $ head rest
      return (e, expr)
soBackquote e [SList _ l] = do
  pairs <- mapM' (soBackquote e . return) l
  return (e, list $ map snd pairs)
    where mapM' :: (SExpr -> Lisp (Env, SExpr)) -> [SExpr] -> Lisp [(Env, SExpr)]
          mapM' f []     = return []
          mapM' f (x:xs) = case x of
            SList _ [SAtom _ (ASymbol "unfold"), arg] -> do
              (_, expr) <- eval e arg
              case expr of
                SList _ l -> do
                  exprs <- mapM (\sexpr -> return (e, sexpr)) l
                  rest <- mapM' f xs
                  return $ exprs ++ rest
                other     -> report (point other) "list expected"
            SList _ (SAtom p (ASymbol "unfold"):_)           -> report p "just one argument required"
            other                                -> do
              result <- f other
              rest   <- mapM' f xs
              return $ result : rest
soBackquote e [arg]        = return (e, arg)
soBackquote _ _            = reportUndef "just one argument required"

-- | special operator interprete
soInterprete :: Env -> [SExpr] -> Lisp (Env, SExpr)
soInterprete e [arg] = do
  str <- getString =<< snd <$> eval e arg
  read <- Reader.read (point arg) str
  expandAndLispScope e read
soInterprete _ _     = reportUndef "just one argument required"

soGensym :: Env -> [SExpr] -> Lisp (Env, SExpr)
soGensym e [] = do
  let (g, sym) = gensym (getG e) e
  return (setG e g, sym)
  where gensym :: Int -> Env -> (Int, SExpr)
        gensym n e = case envLookup ("G-" ++ show n) e of
          Just _  -> gensym (n + 1) e
          Nothing -> (n + 1, symbol $ "G-" ++ show n)
soGensym _ _  = reportUndef "no arguments required"

-- | special operator eval
soLisp :: Env -> [SExpr] -> Lisp (Env, SExpr)
soLisp e args = do
  args' <- mapM (eval e >=> return . snd) args
  expandAndLispScope e args'

-- | converts a string into a symbol
biToSymbol :: [SExpr] -> Lisp SExpr
biToSymbol [exp] = symbol <$> getString exp
biToSymbol _ = reportUndef "just one argument required"

builtinFunctions = [("->symbol", Just 1, biToSymbol)]

specialOperators = [("macroexpand-1", Just 1, soMacroExpand1)
                   ,("macroexpand",   Just 1, soMacroExpand)
                   ,("quote",         Just 1, soQuote)
                   ,("backquote",     Just 1, soBackquote)
                   ,("interprete",    Just 1, soInterprete)
                   ,("gensym",        Just 0, soGensym)
                   ,("eval",          Just 1, soLisp
                    )]
