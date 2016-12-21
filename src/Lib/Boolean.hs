module Lib.Boolean (builtin_not,
                    spop_and,
                    spop_or,
                    spop_impl) where

import SExpr
import qualified Env
import Env (Env)
import Callable

builtin_not :: [SExpr] -> IO SExpr
builtin_not [sexpr] = return . bool . not . from_bool $ sexpr
builtin_not _       = error "'not' requires just one argument"

spop_and :: Eval SExpr -> EvalScope SExpr -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_and eval eval_scope context (x:xs) = do
  (_, expr) <- eval context x
  case from_bool expr of
    True  -> spop_and eval eval_scope context xs
    False -> return (context, bool False)
spop_and _    _          context []     = return (context, bool True)

spop_or :: Eval SExpr -> EvalScope SExpr -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_or eval eval_scope context (x:xs) = do
  (_, expr) <- eval context x
  case from_bool expr of
    True  -> return (context, bool True)
    False -> spop_or eval eval_scope context xs
spop_or _    _          context []     = return (context, bool False)

spop_impl :: Eval SExpr -> EvalScope SExpr -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_impl eval _ context [arg1, arg2] = do
  (_, expr1) <- eval context arg1
  if not $ from_bool expr1
    then return (context, bool True)
    else eval context arg2
spop_impl _    _  _      _            = error "->: two arguments requried"
