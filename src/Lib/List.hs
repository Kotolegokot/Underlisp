module Lib.List (builtin_list,
                 builtin_head,
                 builtin_tail,
                 builtin_init,
                 builtin_last,
                 builtin_length,
                 builtin_append,
                 builtin_nth) where

import Expr
import Lib.Internal

builtin_list :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_list eval context args = do
    exprs <- mapM (eval context) args
    return (SList . fmap fst $ exprs, context)

builtin_head :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_head eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SList xs@(_:_) -> return (head xs, context)
    SList []       -> error "head: empty list"
    _              -> error "list expected"

builtin_head _    _       _     = error "'head' requires just one argument"

builtin_tail :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_tail eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SList xs@(_:_)     -> return (SList $ tail xs, context)
    SList []           -> error "tail: empty list"
    _                  -> error "list expected"
builtin_tail _    _       _     = error "'tail' requires just one argument"

builtin_init :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_init eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SList xs@(_:_) -> return (SList $ init xs, context)
    SList []       -> error "init: empty list"
    _              -> error "list expected"
builtin_init _    _       _     = error "'init' requires just one argument"

builtin_last :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_last eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SList xs@(_:_) -> return (last xs, context)
    SList []       -> error "last: empty list"
    _              -> error "list expected"
builtin_last _    _       _     = error "'last' requires just one argument"

builtin_length :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_length eval context [arg] = do
  (expr, _) <- eval context arg
  case expr of
    SList xs -> return (SInt $ length xs, context)
    _        -> error "list expected"
builtin_length _    _       _     = error "length requires just one argument"

builtin_append :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_append eval context args = helper args []
  where helper (x:xs) acc = do
          (expr, _) <- eval context x
          case expr of
            SList list -> helper xs (acc ++ list)
            _          -> error "list expected"
        helper [] acc     = return (SList acc, context)

builtin_nth :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_nth eval context [arg1, arg2] = do
    (expr1, _) <- eval context arg1
    case expr1 of
      SInt n -> if n < 0 then error "nth: expect positive number" else do
          (expr2, _) <- eval context arg2
          case expr2 of
            SList list -> if length list <= n then error "nth: out of bounds" else return (list !! n, context)
            _          -> error "nth: second argument must be a list"
      _      -> error "nth: first argument must be an int" 
builtin_nth _    _       _            = error "'nth' requires two arguments"
