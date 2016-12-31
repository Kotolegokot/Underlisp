{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Meta (spopMacro,
                 spopMacroExpand,
                 spopQuote,
                 spopBackquote,
                 spopInterprete,
                 spopGensym,
                 spopEval) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Reader
import Base
import Util
import Point
import Exception

-- | special operator macro
-- | (macro lambda-list [body])
spopMacro :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopMacro _ _ e (lambdaList:body) = return (e, callable $ Macro e prototype body [])
  where prototype = parseLambdaList lambdaList
spopMacro _ _ _ []                 = reportUndef "at least one argument requried"

spopMacroExpand :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopMacroExpand eval evalScope e [SList p (first:args)] = do
  (_, first') <- eval e first
  case first' of
    SAtom _ (ACallable (Macro localE prototype sexprs bound)) -> do
      let argBindings = bindArgs prototype (bound ++ args)
      (_, expr) <- evalScope (lappend localE argBindings) sexprs
      return (e, expr)
    _                                                         -> report p "macro invocation expected"
spopMacroExpand _    _          _ [sexpr]              = report (point sexpr) "list expected"
spopMacroExpand _    _          _ _                    = reportUndef "just one argument required"

-- | special operator quote
spopQuote :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopQuote eval _ e [arg] = return (e, arg)
spopQuote _    _ _ _     = reportUndef "just one argument requried"

-- | special operator backquote
spopBackquote :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopBackquote eval evalScope e [SList _ (SAtom p (ASymbol  "interpolate") : rest)]
  | length rest /= 1 = report p "just one argument required"
  | otherwise        = do
      (_, expr) <- eval e $ head rest
      return (e, expr)
spopBackquote eval evalScope e [SList _ l] = do
  pairs <- mapM' (spopBackquote eval evalScope e . return) l
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
spopBackquote _    _          e [arg]        = return (e, arg)
spopBackquote _    _          _ _            = reportUndef "just one argument required"

-- | special operator interprete
spopInterprete :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopInterprete eval evalScope e [arg] = do
  (_, expr) <- eval e arg
  case expr of
    SList p str -> evalScope e . Reader.read p  $ map fromChar str
    _           -> report (point arg) "string expected"
spopInterprete _    _         _ _     = reportUndef "just one argument required"

spopGensym :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopGensym _ _ e [] = do
  let (g, sym) = gensym (getG e) e
  return (setG e g, sym)
  where gensym :: Int -> Env -> (Int, SExpr)
        gensym n e = case envLookup ("G-" ++ show n) e of
          Just _  -> gensym (n + 1) e
          Nothing -> (n + 1, symbol $ "G-" ++ show n)
spopGensym _ _ _ _  = reportUndef "no arguments required"

-- | special operator eval
spopEval :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
spopEval eval _ e [arg] = do
  (_, expr) <- eval e arg
  eval e expr
spopEval _    _ _ _     = reportUndef "just one argument required"
