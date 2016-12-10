module Lib.Meta (builtin_quote,
                 builtin_interprete,
                 builtin_eval) where

import qualified Reader
import Expr
import Lib.Internal

builtin_quote :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_quote eval context [arg] = return (arg, context)
builtin_quote _    _       _     = error "'quote' requires just one argument"

builtin_interprete :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_interprete eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      SString str -> (eval context . Reader.read $ str) >> (return (empty_list, context))
      _           -> error "string expected"
builtin_interprete _    _       _     = error "'interprete' requires just one argument"

builtin_eval :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_eval eval context [arg] = do
    (expr, _) <- eval context arg
    case expr of
      list@(SList _) -> eval context list
      _              -> error "list expected"
builtin_eval _    _       _     = error "'eval' requires just one argument"
