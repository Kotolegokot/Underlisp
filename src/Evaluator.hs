{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Evaluator where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Env
import LexicalEnvironment
import qualified Reader
import Control.Arrow
import Control.Monad (void, foldM)
import Control.Monad (when)
import SExpr
import Util
import Callable
import LispShow
import Lib.Everything
import Point
import Exception

prelude_path = "stdlib/prelude.lisp" :: String

evaluate_program :: [SExpr] -> IO ()
evaluate_program body = do
  prelude <- load_prelude
  void $ eval_scope prelude body

evaluate_module :: [SExpr] -> IO (Map String SExpr)
evaluate_module body = do
  prelude <- load_prelude
  (e, _) <- eval_scope prelude body
  return $ Env.merge e

evaluate_module_no_prelude :: [SExpr] -> IO (Map String SExpr)
evaluate_module_no_prelude body = do
  (e, _) <- eval_scope start_env body
  return $ Env.merge e

-- | evaluates a lexical scope
eval_scope :: LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
eval_scope e = foldM (\(prev_e, _) sexpr -> eval prev_e sexpr) (Env.pass e, nil)

eval :: LEnv SExpr -> SExpr -> IO (LEnv SExpr, SExpr)
eval e (SList p (first:rest))  = do
  (_, first') <- eval e first
  rethrow (\le -> if le_point le == Undefined
                  then le { le_point = p }
                  else le) $ case first' of
    SAtom _ (ACallable (UserDefined local_e prototype sexprs bound))      -> do
      pairs <- mapM (eval e) rest
      let arg_bindings = bind_args prototype (bound ++ map snd pairs)
      (_, expr) <- eval_scope (Env.lappend local_e arg_bindings) sexprs
      return (e, replace_point expr p)
    SAtom _ (ACallable (Macro local_e prototype sexprs bound))            -> do
      let arg_bindings = bind_args prototype (bound ++ rest)
      (_, expr) <- eval_scope (Env.lappend local_e arg_bindings) sexprs
      (e', expr') <- eval e expr
      return (e', replace_point expr' p)
    SAtom _ (ACallable (BuiltIn name _ f bound))                          ->
      rethrow (\le -> le { le_cmd = name }) $ do
        pairs <- mapM (eval e) rest
        result <- f (bound ++ map snd pairs)
        return (e, replace_point result p)
    SAtom _ (ACallable (SpecialOp name _ f bound))                        ->
      rethrow (\le -> le { le_cmd = name }) $ do
        (e', expr) <- f eval eval_scope e (bound ++ rest)
        return (e', replace_point expr p)
    _-> report p $ "unable to execute s-expression: '" ++ lisp_show first' ++ "'"
eval e (SAtom p (ASymbol sym)) = case Env.lookup sym e of
  Just value -> return (e, replace_point value p)
  Nothing    -> report p $ "undefined identificator '" ++ sym ++ "'"
eval e sexpr                   = return (e, sexpr)

load_prelude :: IO (LEnv SExpr)
load_prelude = do
  text <- readFile prelude_path
  (e, _) <- eval_scope start_env $ Reader.read (start_point prelude_path) text
  return e

start_env :: LEnv SExpr
start_env = Env.fromList $
    (fmap (\(name, args, f) -> (name, callable $ SpecialOp name args f [])) [
    ("gensym",                       Just 0,  spop_gensym),
    ("let",                          Nothing, spop_let),
    ("if",                           Just 3,  spop_if),
    ("define",                       Just 2,  spop_define),
    ("lambda",                       Nothing, spop_lambda),
    ("macro",                        Nothing, spop_macro),
    ("macro-expand",                 Just 1,  spop_macro_expand),
    ("quote",                        Just 1,  spop_quote),
    ("backquote",                    Just 1,  spop_backquote),
    ("interprete",                   Just 1,  spop_interprete),
    ("eval",                         Just 1,  spop_eval),
    ("and",                          Nothing, spop_and),
    ("or",                           Nothing, spop_or),
    ("->",                           Just 2,  spop_impl),
    ("env",                          Nothing, spop_env),
    ("load-env",                     Just 1,  spop_load_env),
    ("import-env",                   Just 1,  spop_import_env),
    ("current-env",                  Just 0,  spop_current_env),
    ("env-from-file",                Just 1,  spop_env_from_file),
    ("env-from-file-no-prelude",     Just 1,  spop_env_from_file_no_prelude),
    ("defined?",                     Just 1,  spop_defined),
    ("seq",                          Nothing, spop_seq) ]) ++
  (fmap (\(name, args, f) -> (name, callable $ BuiltIn name args f [])) [
    ("type",             Just 1,  builtin_type),
    ("bind",             Nothing, builtin_bind),
    ("put-char",         Just 1,  builtin_put_char),
    ("write",            Just 1,  builtin_write),
    ("flush",            Just 0,  builtin_flush),
    ("get-line",         Just 0,  builtin_get_line),
    ("list",             Nothing, builtin_list),
    ("head",             Just 1,  builtin_head),
    ("tail",             Just 1,  builtin_tail),
    ("null",             Just 1,  builtin_null),
    ("append",           Just 2,  builtin_append),
    ("+",                Nothing, builtin_sum),
    ("-",                Just 2,  builtin_substract),
    ("*",                Nothing, builtin_product),
    ("/",                Just 2,  builtin_divide),
    ("float",            Just 1,  builtin_float),
    ("not",              Just 1,  builtin_not),
    ("=",                Just 2,  builtin_eq),
    ("<",                Just 2,  builtin_lt),
    ("error",            Just 1,  builtin_error),
    ("initial-env",      Just 0,  builtin_initial_env),
    ("function-env",     Just 1,  builtin_function_env) ])

spop_env_from_file :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_env_from_file eval eval_scope e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SList p l -> do
      let filename = map from_char l
      text <- readFile filename
      e' <- evaluate_module $ Reader.read p text
      return (e, env e')
    other     -> report (point other) "string expected"
spop_env_from_file _    _          _        _    = report_undef "just one argument required"

spop_env_from_file_no_prelude :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_env_from_file_no_prelude eval eval_scope e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SList p list -> do
      let filename = map from_char list
      text <- readFile filename
      e' <- evaluate_module_no_prelude $ Reader.read p text
      return (e, env e')
    other        -> report (point other) "string expected"
spop_env__from_file_no_prelude _    _          _       _      = report_undef "just one argument required"

builtin_initial_env :: [SExpr] -> IO SExpr
builtin_initial_env [] = do
  prelude <- load_prelude
  return . env $ Env.merge prelude
builtin_initial_env _  = report_undef "no arguments requried"
