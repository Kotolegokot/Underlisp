module Lib.Meta (builtinFunctions
                ,specialOperators) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Reader
import Base
import Util
import Point
import Exception

soMacroExpand :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soMacroExpand eval evalScope e [sexpr] = do
  (_, sexpr') <- eval e sexpr
  expanded <- expandMacros e [sexpr']
  return (e, head expanded)

soMacroExpand1 :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soMacroExpand1 eval evalScope e [sexpr] = do
  (_, evaluated) <- eval e sexpr
  soMacroExpand1' evaluated
    where soMacroExpand1' l@(SList _ (SAtom _ (ASymbol sym):args)) = case lookupMacro sym e of
            Just m   -> do
              result <- callMacro expandAndEvalScope e m args
              return (e, result)
            Nothing  -> return (e, l)
          soMacroExpand1' other                               = return (e, other)
soMacroExpand1 _    _         _ _       = reportUndef "just one argument required"

-- | special operator quote
soQuote :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soQuote eval _ e [arg] = return (e, arg)
soQuote _    _ _ _     = reportUndef "just one argument requried"

-- | special operator backquote
soBackquote :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soBackquote eval evalScope e [SList _ (SAtom p (ASymbol  "interpolate") : rest)]
  | length rest /= 1 = report p "just one argument required"
  | otherwise        = do
      (_, expr) <- eval e $ head rest
      return (e, expr)
soBackquote eval evalScope e [SList _ l] = do
  pairs <- mapM' (soBackquote eval evalScope e . return) l
  return (e, list $ map snd pairs)
    where mapM' :: (SExpr -> IO (Env, SExpr)) -> [SExpr] -> IO [(Env, SExpr)]
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
soBackquote _    _          e [arg]        = return (e, arg)
soBackquote _    _          _ _            = reportUndef "just one argument required"

-- | special operator interprete
soInterprete :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soInterprete eval evalScope e [arg] = do
  (_, expr) <- eval e arg
  case expr of
    SList p str -> evalScope e . Reader.read p  $ map fromChar str
    _           -> report (point arg) "string expected"
soInterprete _    _         _ _     = reportUndef "just one argument required"

soGensym :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soGensym _ _ e [] = do
  let (g, sym) = gensym (getG e) e
  return (setG e g, sym)
  where gensym :: Int -> Env -> (Int, SExpr)
        gensym n e = case envLookup ("G-" ++ show n) e of
          Just _  -> gensym (n + 1) e
          Nothing -> (n + 1, symbol $ "G-" ++ show n)
soGensym _ _ _ _  = reportUndef "no arguments required"

-- | special operator eval
soEval :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soEval eval _ e [arg] = do
  (_, expr) <- eval e arg
  eval e expr
soEval _    _ _ _     = reportUndef "just one argument required"

builtinFunctions = []

specialOperators = [("macroexpand-1", Just (1 :: Int), soMacroExpand1)
                   ,("macroexpand",   Just 1,          soMacroExpand)
                   ,("quote",         Just 1,          soQuote)
                   ,("backquote",     Just 1,          soBackquote)
                   ,("interprete",    Just 1,          soInterprete)
                   ,("gensym",        Just 0,          soGensym)
                   ,("eval",          Just 1,          soEval)]
