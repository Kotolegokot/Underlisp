{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Meta (spop_macro,
                 spop_quote,
                 spop_backquote,
                 spop_interprete,
                 spop_eval) where

import Data.List (delete, elemIndices)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Env
import Env (Env)
import qualified Reader
import SExpr
import Util
import LexicalEnvironment
import Callable

-- | special operator macro
-- | (macro lambda-list [body])
spop_macro :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_macro _ _ e (lambda_list:body) = return (e, callable $ Macro e prototype body [])
  where prototype = parse_lambda_list lambda_list
spop_macro _ _ _ []                 = error "macro: at least one argument requried"

-- | takes an s-list of the form (arg1 arg2... [&rst argLast])
-- | and constructs a Prototype
parse_lambda_list :: SExpr -> Prototype
parse_lambda_list (SList lambda_list)
  | not $ all is_symbol lambda_list = error "all items in a lambda list must be symbols"
  | length ixs > 1                  = error "more than one &rest in a lambda list is forbidden"
  | rest && ix /= count - 2         = error "&rest must be last but one"
  | otherwise                       = if rest
                                      then Prototype (delete "&rest" . map from_symbol $ lambda_list) rest
                                      else Prototype (map from_symbol $ lambda_list) rest
  where ixs   = elemIndices (symbol "&rest") lambda_list
        ix    = head ixs
        rest  = length ixs == 1
        count = length lambda_list
parse_lambda_list _ = error "lambda list must be a list"

-- | special operator quote
spop_quote :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_quote eval _ context [arg] = return (context, arg)
spop_quote _    _ _       _     = error "quote: just one argument requried"

-- | special operator backquote
spop_backquote :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_backquote eval eval_scope context [SList (SAtom (ASymbol  "interpolate") : rest)]
  | length rest /= 1 = error "interpolate: just one argument required"
  | otherwise        = do
      (_, expr) <- eval context $ head rest
      return (context, expr)
spop_backquote eval eval_scope context [SList list] = do
  pairs <- mapM' (spop_backquote eval eval_scope context . return) list
  return (context, SList $ map snd pairs)
    where mapM' :: (SExpr -> IO (LEnv SExpr, SExpr)) -> [SExpr] -> IO [(LEnv SExpr, SExpr)]
          mapM' f []     = return []
          mapM' f (x:xs) = case x of
            SList [SAtom (ASymbol "unfold"), arg] -> do
              (_, expr) <- eval context arg
              case expr of
                SList list -> do
                  exprs <- mapM (\sexpr -> return (context, sexpr)) list
                  rest <- mapM' f xs
                  return $ exprs ++ rest
                _          -> error "unfold: list expected"
            SList (SAtom (ASymbol "unfold"):_)           -> error "unfold: just one argument required"
            other                                -> do
              result <- f other
              rest   <- mapM' f xs
              return $ result : rest
spop_backquote _    _          context [arg]        = return (context, arg)
spop_backquote _    _          _       _            = error "backquote: just one argument required"

-- | special operator interprete
spop_interprete :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_interprete eval eval_scope context [arg] = do
  (_, expr) <- eval context arg
  case expr of
    SAtom (AString str) -> eval_scope context . Reader.read Undefined  $ str -- TODO: change Undefined to a normal point
    _                   -> error "interprete: string expected"
spop_interprete _    _          _       _     = error "interprete: just one argument required"

-- | special operator eval
spop_eval :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_eval eval _ context [arg] = do
  (_, expr) <- eval context arg
  eval context expr
spop_eval _    _ _       _     = error "eval: just one argument required"
