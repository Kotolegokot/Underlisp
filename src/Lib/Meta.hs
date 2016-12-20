module Lib.Meta (spop_quote,
                 spop_backquote,
                 spop_interprete,
                 spop_eval) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Env
import Env (Env)
import qualified Reader
import SExpr
import Util
import Lib.Internal

-- special operator quote
spop_quote :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_quote eval _ context [arg] = return (context, arg)
spop_quote _    _ _       _     = error "quote: just one argument requried"

-- | special operator backquote
spop_backquote :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_backquote eval eval_scope context [SList (SSymbol "interpolate" : rest)]
  | length rest /= 1 = error "interpolate: just one argument required"
  | otherwise        = do
      (_, expr) <- eval context $ head rest
      return (context, expr)
spop_backquote eval eval_scope context [SList list] = do
  pairs <- mapM' (spop_backquote eval eval_scope context . return) list
  return (context, SList $ map snd pairs)
    where mapM' :: (SExpr -> IO (Env SExpr, SExpr)) -> [SExpr] -> IO [(Env SExpr, SExpr)]
          mapM' f []     = return []
          mapM' f (x:xs) = case x of
            SList [SSymbol "unfold", arg] -> do
              (_, expr) <- eval context arg
              case expr of
                SList list -> do
                  exprs <- mapM (\sexpr -> return (context, sexpr)) list
                  rest <- mapM' f xs
                  return $ exprs ++ rest
                _          -> error "unfold: list expected"
            SList (SSymbol "unfold":_)           -> error "unfold: just one argument required"
            other                                -> do
              result <- f other
              rest   <- mapM' f xs
              return $ result : rest
spop_backquote _    _          context [arg]        = return (context, arg)
spop_backquote _    _          _       _            = error "backquote: just one argument required"

-- | special operator interprete
spop_interprete :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_interprete eval eval_scope context [arg] = do
  (_, expr) <- eval context arg
  case expr of
    SString str -> eval_scope context . Reader.read Undefined  $ str -- TODO: change Undefined to a normal point
    _           -> error "interprete: string expected"
spop_interprete _    _          _       _     = error "interprete: just one argument required"

-- | special operator eval
spop_eval :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_eval eval _ context [arg] = do
  (_, expr) <- eval context arg
  eval context expr
spop_eval _    _ _       _     = error "eval: just one argument required"
