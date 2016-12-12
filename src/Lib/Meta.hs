module Lib.Meta (spop_macro,
                 spop_quote,
                 spop_backquote,
                 spop_interprete,
                 spop_eval) where

import qualified Reader
import SExpr
import Lib.Internal

spop_macro :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_macro eval context (lambda_list:body) = return (SCallable macro, context)
  where (arg_names, rest) = handle_lambda_list lambda_list
        macro = Macro arg_names rest (SList $ SSymbol "seq" : body)
spop_macro _   _       _                   = error "macro: at least one argument required"

spop_quote :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_quote eval context [arg] = return (arg, context)
spop_quote _    _       _     = error "'quote' requires just one argument"

spop_backquote :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_backquote eval context [SList (SSymbol "interpolate" : rest)]
  | length rest /= 1 = error "interpolate: just one argument required"
  | otherwise        = do
      (expr, _) <- eval context (head rest)
      return (expr, context)
spop_backquote eval context [SList list] = do
    pairs <- mapM' (spop_backquote eval context . return) list
    return (SList (map fst pairs), context)
        where mapM' :: (SExpr -> IO (SExpr, Context)) -> [SExpr] -> IO [(SExpr, Context)]
              mapM' f [] = return []
              mapM' f (x:xs) = case x of
                                 SList [SSymbol "unfold", arg] -> do
                                     (expr, _) <- eval context arg
                                     case expr of
                                       SList list -> do
                                         exprs <- mapM' f list
                                         rest <- mapM' f xs
                                         return $ exprs
                                       _           -> error "unfold: list expected"
                                 SList (SSymbol "unfold":_)           -> error "unfold: just one argument required"
                                 other                                -> do
                                     result <- f other
                                     rest   <- mapM' f xs
                                     return $ result : rest
spop_backquote eval context [arg] = return (arg, context)
spop_backquote eval context _     = error "backquote: just one argument required"

spop_interprete :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_interprete eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      SString str -> eval context . Reader.read $ str
      _           -> error "interprete: string expected"
spop_interprete _    _       _     = error "interprete: just one argument required"

spop_eval :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_eval eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      list@(SList _) -> eval context list
      _              -> error "list expected"
spop_eval _    _       _     = error "'eval' requires just one argument"
