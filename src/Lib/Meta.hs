module Lib.Meta (spop_macro,
                 spop_macro_expand,
                 spop_quote,
                 spop_backquote,
                 spop_interprete,
                 spop_eval) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Reader
import SExpr
import Util
import Lib.Internal

-- special operator macro
spop_macro :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_macro eval context (lambda_list:body) = return (SCallable macro, context)
  where (arg_names, rest) = handle_lambda_list lambda_list
        macro = Macro context (Prototype arg_names rest) body []
spop_macro _   _       _                   = error "macro: at least one argument required"

-- special operator macro-expand
-- TODO: remove
spop_macro_expand :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_macro_expand eval context (name:args) = do
  (expr, _) <- eval context name
  case expr of
    SCallable (Macro l_context (Prototype arg_names rest) sexprs bound) -> do
        let f_context = handle_args arg_names rest args
--        (expr, _) <- eval (f_context `Map.union` l_context) sexpr
        return (nil, context)
    _                                     -> error "macro-expand: macro expected"
spop_macro_expand _    _       _           = error "macro-expand: at least one argument required"

handle_args :: [String] -> Bool -> [SExpr] -> Context
handle_args arg_names False args
  | length arg_names > length args = error "too little arguments"
  | length arg_names < length args = error "too many arguements"
  | otherwise                    = foldr (\(name, value) context -> Map.insert name value context) Map.empty (zip arg_names args)
handle_args arg_names True args
  | length arg_names - 1 > length args = error "too little arguments"
  | otherwise                          = let (left, right) = splitAt (length arg_names - 1) args
                                          in let args' = left ++ [SList right]
                                              in foldr (\(name, value) context -> Map.insert name value context) Map.empty (zip arg_names args')

-- special operator quote
spop_quote :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_quote eval context [arg] = return (arg, context)
spop_quote _    _       _     = error "'quote' requires just one argument"

-- special operator backquote
spop_backquote :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_backquote eval context [SList (SSymbol "interpolate" : rest)]
  | length rest /= 1 = error "interpolate: just one argument required"
  | otherwise        = do
      (expr, _) <- eval context $ head rest
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
                                         exprs <- mapM (\sexpr -> return (sexpr, context)) list
                                         rest <- mapM' f xs
                                         return $ exprs ++ rest
                                       _           -> error "unfold: list expected"
                                 SList (SSymbol "unfold":_)           -> error "unfold: just one argument required"
                                 other                                -> do
                                     result <- f other
                                     rest   <- mapM' f xs
                                     return $ result : rest
spop_backquote eval context [arg] = return (arg, context)
spop_backquote eval context _     = error "backquote: just one argument required"

-- special operator interprete
spop_interprete :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_interprete eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      SString str -> eval_list eval context . Reader.read Undefined  $ str -- TODO: change Undefined to a normal point
      _           -> error "interprete: string expected"
spop_interprete _    _       _     = error "interprete: just one argument required"

-- special operator eval
spop_eval :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_eval eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      list@(SList _) -> eval context list
      _              -> error "list expected"
spop_eval _    _       _     = error "'eval' requires just one argument"
