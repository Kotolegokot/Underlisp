module Lib.Main (builtin_let,
                 builtin_lambda,
                 builtin_define,
                 builtin_type) where

import qualified Data.Map as Map
import Data.List (elemIndex)
import SExpr
import Lib.Internal

builtin_let :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_let eval context ((SList pairs):body) = do
    new_context <- handle_pairs pairs context
    eval new_context (SList (SKeyword "seq":body))
        where handle_pairs (x:xs) acc = case x of
                                          (SList [SKeyword var, value]) -> do
                                              (expr, _) <- eval acc value
                                              handle_pairs xs (Map.insert var expr acc)
                                          (SList [_, _]) -> error "first item in a let binding pair must be a keyword"
                                          _              -> error "a binding in 'let' must be of the following form: (var value)"
              handle_pairs []     acc = return acc
builtin_let _    _       _               = error "list of bindings expected"

builtin_lambda :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_lambda eval context (SList args:body)
  | not $ all is_keyword args = error "every argument in 'define' must be a keyword"
  | otherwise                 = return (SFunc (UserDefined (length args) (FList $ FKeyword "seq" : fmap handle_sexpr body)), context)
    where handle_sexpr (SList list)         = FList . fmap handle_sexpr $ list
          handle_sexpr kword@(SKeyword str) = case elemIndex kword args of
                                                Just index -> FRef index
                                                Nothing    -> FKeyword str
          handle_sexpr sexpr                = sexpr2fexpr sexpr
builtin_lambda _    _       (_:_) = error "first argument of 'define' must be an argument list"
builtin_lambda _    _       _     = error "'define' requires at least one argument"

builtin_define :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_define eval context (first:second:body)
  | not $ is_keyword first                    = error "first argument of 'define' must be a keyword"
  | not $ is_list second                      = error "second argument of 'define' must be a list"
  | not . all is_keyword . from_list $ second = error "arguments in 'define' must be keywords"
  | otherwise                                 = return (empty_list, Map.insert name (SFunc func) context)
      where name = from_keyword first
            args = from_list second
            func = UserDefined (length args) (FList $ FKeyword "seq" : fmap handle_sexpr body)
            handle_sexpr (SList list)         = FList . fmap handle_sexpr $ list
            handle_sexpr kword@(SKeyword str) = case elemIndex kword args of
                                                  Just index -> FRef index
                                                  Nothing    -> FKeyword str
            handle_sexpr sexpr                = sexpr2fexpr sexpr
builtin_define _    _       _                = error "'define' requires at least two arguments"

builtin_type :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_type eval context [arg] = do
    (expr, _) <- eval context arg
    return (SString $ show_type expr, context)
builtin_type _    _       _     = error "'type' requires just one argument"

