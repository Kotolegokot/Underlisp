{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Evaluator (evaluate_program, evaluate_module) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Env
import LexicalEnv
import qualified Reader
import Data.List (elemIndices, delete)
import Control.Arrow
import Control.Monad (void, foldM)
import Control.Monad (when)
import SExpr
import Util
import Callable
import LispShow
import Lib.Everything

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
-- | 1. expands all macros in it
-- | 2. collect all function definitions
-- | 3. executes the remaining s-expressions successively
eval_scope :: LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
eval_scope env sexprs = do
  (env', sexprs') <- expand_macros (Env.pass env) sexprs
  eval_functions env' sexprs'

-- | looks through a lexical scope, executes all defmacros,
-- | and expands them
expand_macros :: LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, [SExpr])
expand_macros e sexprs = do
  let e' = collect_macros e sexprs
  sexprs' <- apply_macros e' $ filter (not . is_defmacro) sexprs
  return (e', sexprs')
  where is_defmacro :: SExpr -> Bool
        is_defmacro (SList (SAtom (ASymbol "defmacro"):_)) = True
        is_defmacro _                                      = False

-- | looks through a lexical scope and evaluates all defmacros
collect_macros :: LEnv SExpr -> [SExpr] -> LEnv SExpr
collect_macros e sexprs = Env.lappend e add_lexical'
  where add_lexical = foldl (\acc sexpr
                              -> case sexpr of
                                   SList (SAtom (ASymbol "defmacro"):definition)
                                     -> let (name, macro) = handle_defmacro e definition
                                        in   Map.insert name (callable macro) acc
                                   _ -> acc)
                      Map.empty
                      sexprs
        add_lexical' = fmap (\(SAtom (ACallable (Macro e prototype sexprs bound))) ->
                               callable $ Macro (Env.lappend e add_lexical') prototype sexprs bound) add_lexical

data State = Default | Quote | Backquote

-- | applies macros in a certain lexical scope
-- | all macros are stored in Env SExpr
apply_macros :: LEnv SExpr -> [SExpr] -> IO [SExpr]
apply_macros e sexprs = mapM (apply_macro Default e) sexprs
  where apply_macro :: State -> LEnv SExpr -> SExpr -> IO SExpr
        apply_macro Default e (SList (SAtom (ASymbol "backquote"):body)) = do
          body' <- mapM (apply_macro Backquote e) body
          return $ SList (symbol "backquote" : body')

        apply_macro Default e (SList (SAtom (ASymbol "quote"):body)) = do
          body' <- mapM (apply_macro Quote e) body
          return $ SList (symbol "quote" : body')

        apply_macro Default e list@(SList (SAtom (ASymbol sym):body)) = case Env.lookup sym e of
          Just (SAtom (ACallable (Macro e prototype sexprs bound))) -> do
            body' <- mapM (apply_macro Default e) body
            let arg_bindings = bind_args prototype (bound ++ body')
            (_, macro_expr) <- eval_scope (Env.lappend e arg_bindings) sexprs
            apply_macro Default e macro_expr
          _                                                                 -> do
            list' <- mapM (apply_macro Default e) (from_list list)
            return $ SList list'

        apply_macro Default e        other                      = return other

        apply_macro Quote    _       sexpr                      = return sexpr

        apply_macro Backquote e (SList (SAtom (ASymbol "interpolate"):body)) = do
          body' <- mapM (apply_macro Default e) body
          return $ SList (symbol "interpolate" : body')

        apply_macro Backquote env other                                = return other
-- | takes a list of the form (name (arg1 arg2... [&rest argLast]) body...)
-- | and constructs a Macro object (Callable)
-- | also returns its name
handle_defmacro :: LEnv SExpr -> [SExpr] -> (String, Callable LEnv SExpr)
handle_defmacro context (s_name:s_lambda_list:body)
  | not $ is_symbol s_name = error "macro name must be a symbol"
  | otherwise              = (name, Macro context prototype body [])
  where name      = from_symbol s_name
        prototype = parse_lambda_list s_lambda_list

-- | looks through a lexical scope, executes all defines,
-- | and evaluates the remaining s-expressions
eval_functions :: LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
eval_functions context sexprs = do
  let context' = collect_functions context sexprs
  sexpr' <- apply_functions context' $ filter (not . is_define) sexprs
  return (context', sexpr')
  where is_define :: SExpr -> Bool
        is_define (SList (SAtom (ASymbol "define"):_)) = True
        is_define _                                    = False

-- | looks through a lexical scope and evaluates all defines
collect_functions :: LEnv SExpr -> [SExpr] -> LEnv SExpr
collect_functions e sexprs = Env.lappend e add
  where add = foldl (\acc sexpr
                      -> case sexpr of
                           SList (SAtom (ASymbol "define"):definition)
                             -> let (name, function) = handle_define e definition
                                in  Map.insert name (callable function) acc
                           _ -> acc)
              Map.empty
              sexprs

-- | evaluates functions in a certain lexical scope
apply_functions :: LEnv SExpr -> [SExpr] -> IO SExpr
apply_functions env sexprs = do
  (_, sexpr) <- foldM (\(prev_env, _) sexpr -> eval prev_env sexpr) (env, nil) sexprs
  return sexpr

-- | takes an s-list of the form (name (arg1 arg2... [&rest lastArg]) body...)
-- | and constructs the correspoding UserDefined object (Callable)
-- | also returns its name
handle_define :: LEnv SExpr -> [SExpr] -> (String, Callable LEnv SExpr)
handle_define context (s_name:s_lambda_list:body)
  | not $ is_symbol s_name = error "function name must be a symbol"
  | otherwise              = (name, UserDefined context prototype body [])
  where name      = from_symbol s_name
        prototype = parse_lambda_list s_lambda_list

eval :: LEnv SExpr -> SExpr -> IO (LEnv SExpr, SExpr)
eval e (SList (first:rest))  = do
  (_, first') <- eval e first
  case first' of
    SAtom (ACallable (UserDefined local_env prototype sexprs bound)) -> do
      when (is_symbol first) $
        when (from_symbol first == "factorial") (lisp_print local_env)
      pairs <- mapM (eval e) rest
      let arg_bindings = bind_args prototype (bound ++ map snd pairs)
      (_, expr) <- eval_scope (Env.lappend e arg_bindings) sexprs
      return (e, expr)
    SAtom (ACallable (BuiltIn _ _ f bound))                          -> do
      pairs <- mapM (eval e) rest
      result <- f (bound ++ map snd pairs)
      return (e, result)
    SAtom (ACallable (SpecialOp _ _ f bound))                        -> f eval eval_scope e (bound ++ rest)
    _                                                                -> error $ "unable to execute s-expression: '" ++ lisp_show first' ++ "'"
eval e (SAtom (ASymbol sym)) = case Env.lookup sym e of
  Just value -> return (e, value)
  Nothing   -> error $ "undefined identificator '" ++ sym ++ "'"
eval e sexpr                 = return (e, sexpr)

load_prelude :: IO (LEnv SExpr)
load_prelude = do
  text <- readFile "stdlib/prelude.lisp"
  (e, _) <- eval_scope start_env $ Reader.read Undefined text -- TODO: change Undefined
  return e

start_env :: LEnv SExpr
start_env = Env.fromList $
    (fmap (\(name, args, f) -> (name, callable $ SpecialOp name args f [])) [
    ("let",                          Nothing, spop_let),
    ("if",                           Just 3,  spop_if),
    ("defvar",                       Just 2,  spop_defvar),
    ("quote",                        Just 1,  spop_quote),
    ("backquote",                    Just 1,  spop_backquote),
    ("interprete",                   Just 1,  spop_interprete),
    ("eval",                         Just 1,  spop_eval),
    ("and",                          Nothing, spop_and),
    ("or",                           Nothing, spop_or),
    ("->",                           Just 2,  spop_impl),
    ("context",                      Nothing, spop_context),
    ("load-context",                 Just 1,  spop_load_context),
    ("current-context",              Just 0,  spop_current_context),
    ("context-from-file",            Just 1,  spop_context_from_file),
    ("context-from-file-no-prelude", Just 1,  spop_context_from_file_no_prelude),
    ("seq",                          Nothing, spop_seq) ]) ++
  (fmap (\(name, args, f) -> (name, callable $ BuiltIn name args f [])) [
    ("type",         Just 1,  builtin_type),
    ("bind",         Nothing, builtin_bind),
    ("print",        Nothing, builtin_print),
    ("print-ln",     Nothing, builtin_print_ln),
    ("flush",        Just 0,  builtin_flush),
    ("get-line",     Just 0,  builtin_get_line),
    ("list",         Nothing, builtin_list),
    ("head",         Just 1,  builtin_head),
    ("tail",         Just 1,  builtin_tail),
    ("null",         Just 1,  builtin_null),
    ("append",       Just 2,  builtin_append),
    ("+",            Nothing, builtin_sum),
    ("-",            Just 2,  builtin_substract),
    ("*",            Nothing, builtin_product),
    ("/",            Just 2,  builtin_divide),
    ("float",        Just 1,  builtin_float),
    ("not",          Just 1,  builtin_not),
    ("=",            Just 2,  builtin_eq),
    ("<",            Just 2,  builtin_lt),
    ("concat",       Nothing, builtin_concat),
    ("str-to-int",   Just 1,  builtin_str_to_int),
    ("str-to-float", Just 1,  builtin_str_to_float),
    ("str-length",   Just 1,  builtin_str_length),
    ("error",        Just 1,  builtin_error) ])

spop_context_from_file :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_context_from_file eval eval_scope e [arg] = do
  (_, sexpr) <- eval e arg
  putStrLn $ "FILE: " ++ from_string sexpr
  case sexpr of
    SAtom (AString filename) -> do
      text <- readFile filename
      e' <- evaluate_module $ Reader.read Undefined text -- TODO: change Undefined
      when (filename == "stdlib/ord.lisp") $
        lisp_print e'
      return (e, env e')
    _                        -> error "context-from-file: string expected"
spop_context_from_file _    _          _        _    = error "context-from-file: just one argument required"

spop_context_from_file_no_prelude :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_context_from_file_no_prelude eval eval_scope e [arg] = do
  (_, sexpr) <- eval e arg
  case sexpr of
    SAtom (AString filename) -> do
      text <- readFile filename
      e' <- evaluate_module_no_prelude $ Reader.read Undefined text -- TODO: change Undefined
      return (e, env e')
    _                        -> error "context-from-file-no-prelude: string expected"
spop_context_from_file_no_prelude _    _          _       _      = error "context-from-file-no-prelude: just one argument required"

-- | creates argument bindings from a Prototype
-- | and arguments (s-expressions)
bind_args :: Prototype -> [SExpr] -> Map String SExpr
bind_args (Prototype arg_names False) args
  | length arg_names > length args = error "too little arguments"
  | length arg_names < length args = error "too many arguments"
  | otherwise                      = foldl (\context (name, value) -> Map.insert name value context) Map.empty (zip arg_names args)
bind_args (Prototype arg_names True) args
  | length arg_names - 1 > length args = error "too little arguments"
  | otherwise                          = let (left, right) = splitAt (length arg_names - 1) args
                                             args'         = left ++ [SList right]
                                         in foldl (\context (name, value) -> Map.insert name value context) Map.empty (zip arg_names args')

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
