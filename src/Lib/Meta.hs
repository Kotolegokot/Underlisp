module Lib.Meta (spop_quote,
                 spop_interprete,
                 spop_eval) where

import qualified Reader
import Expr
import Lib.Internal

spop_quote :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_quote eval context [arg] = return (arg, context)
spop_quote _    _       _     = error "'quote' requires just one argument"

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
