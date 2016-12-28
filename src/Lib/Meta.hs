{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Meta (spop_macro,
                 spop_macro_expand,
                 spop_quote,
                 spop_backquote,
                 spop_interprete,
                 spop_eval) where

import Data.List (delete, elemIndices)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Env
import Env (Env)
import qualified Reader
import SExpr
import Util
import LexicalEnvironment
import Callable
import Point
import Exception

-- | special operator macro
-- | (macro lambda-list [body])
spop_macro :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_macro _ _ e (lambda_list:body) = return (e, callable $ Macro e prototype body [])
  where prototype = parse_lambda_list lambda_list
spop_macro _ _ _ []                 = report_undef "at least one argument requried"

-- | takes an s-list of the form (arg1 arg2... [&rst argLast])
-- | and constructs a Prototype
parse_lambda_list :: SExpr -> Prototype
parse_lambda_list (SList p lambda_list)
  | not $ all is_symbol lambda_list = report p "all items in a lambda list must be symbols"
  | length ixs > 1                  = report p "more than one &rest in a lambda list is forbidden"
  | rest && ix /= count - 2         = report p "&rest must be last but one"
  | otherwise                       = if rest
                                      then Prototype (delete "&rest" . map from_symbol $ lambda_list) rest
                                      else Prototype (map from_symbol $ lambda_list) rest
  where ixs   = elemIndices (symbol "&rest") lambda_list
        ix    = head ixs
        rest  = length ixs == 1
        count = length lambda_list
parse_lambda_list _ = report_undef "lambda list must be a list"

spop_macro_expand :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_macro_expand eval eval_scope e [SList p (first:args)] = do
  (_, first') <- eval e first
  case first' of
    SAtom _ (ACallable (Macro local_e prototype sexprs bound)) -> do
      let arg_bindings = bind_args prototype (bound ++ args)
      (_, expr) <- eval_scope (Env.lappend local_e arg_bindings) sexprs
      return (e, expr)
    _                                                          -> report p "macro invocation expected"
spop_macro_expand _    _          _ [sexpr]              = report (point sexpr) "list expected"
spop_macro_expand _    _          _ _                    = report_undef "just one argument required"

-- | special operator quote
spop_quote :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_quote eval _ context [arg] = return (context, arg)
spop_quote _    _ _       _     = report_undef "just one argument requried"

-- | special operator backquote
spop_backquote :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_backquote eval eval_scope e [SList _ (SAtom p (ASymbol  "interpolate") : rest)]
  | length rest /= 1 = report p "just one argument required"
  | otherwise        = do
      (_, expr) <- eval e $ head rest
      return (e, expr)
spop_backquote eval eval_scope e [SList _ l] = do
  pairs <- mapM' (spop_backquote eval eval_scope e . return) l
  return (e, list $ map snd pairs)
    where mapM' :: (SExpr -> IO (LEnv SExpr, SExpr)) -> [SExpr] -> IO [(LEnv SExpr, SExpr)]
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
spop_backquote _    _          context [arg]        = return (context, arg)
spop_backquote _    _          _       _            = report_undef "just one argument required"

-- | special operator interprete
spop_interprete :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_interprete eval eval_scope context [arg] = do
  (_, expr) <- eval context arg
  case expr of
    SList p str -> eval_scope context . Reader.read p  $ map from_char str
    _           -> report (point arg) "string expected"
spop_interprete _    _          _       _     = report_undef "just one argument required"

-- | special operator eval
spop_eval :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_eval eval _ context [arg] = do
  (_, expr) <- eval context arg
  eval context expr
spop_eval _    _ _       _     = report_undef "just one argument required"
