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

-- | special operator macro
-- | (macro lambda-list [body])
soMacro :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soMacro _ _ e (lambdaList:body) = return (e, procedure $ Macro e prototype body [])
  where prototype = parseLambdaList lambdaList
soMacro _ _ _ []                 = reportUndef "at least one argument requried"

soMacroExpand :: Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)
soMacroExpand eval evalScope e [SList p (first:args)] = do
  (_, first') <- eval e first
  case first' of
    SAtom _ (AProcedure (Macro localE prototype sexprs bound)) -> do
      let argBindings = bindArgs prototype (bound ++ args)
      (_, expr) <- evalScope (lappend localE argBindings) sexprs
      return (e, expr)
    _                                                         -> report p "macro invocation expected"
soMacroExpand _    _          _ [sexpr]              = report (point sexpr) "list expected"
soMacroExpand _    _          _ _                    = reportUndef "just one argument required"

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

specialOperators = [("macro",        Nothing,         soMacro)
                   ,("macro-expand", Just (1 :: Int), soMacroExpand)
                   ,("quote",        Just 1,          soQuote)
                   ,("backquote",    Just 1,          soBackquote)
                   ,("interprete",   Just 1,          soInterprete)
                   ,("gensym",       Just 0,          soGensym)
                   ,("eval",         Just 1,          soEval)]
