module Evaluator (evaluate_program, evaluate_module) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Env
import Env (Env)
import qualified Reader
import Data.List (elemIndices, delete)
import Control.Arrow
import Control.Monad (void, foldM)
import SExpr
import Util
import Lib.Everything

evaluate_program :: [SExpr] -> IO ()
evaluate_program body = do
  prelude <- load_prelude
  void $ eval_scope prelude body

evaluate_module :: [SExpr] -> IO (Map String SExpr)
evaluate_module body = do
  prelude <- load_prelude
  (env, _) <- eval_scope prelude body
  return $ Env.merge env

evaluate_module_no_prelude :: [SExpr] -> IO (Map String SExpr)
evaluate_module_no_prelude body = do
  (env, _) <- eval_scope start_env body
  return $ Env.merge env

-- | evaluates a lexical scope
-- | 1. expands all macros in it
-- | 2. collect all function definitions
-- | 3. executes the remaining s-expressions successively
eval_scope :: Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
eval_scope env sexprs = do
  (env', sexprs') <- expand_macros (Env.pass env) sexprs
  eval_functions env' sexprs'

-- | looks through a lexical scope, executes all defmacros,
-- | and expands them
expand_macros :: Env SExpr -> [SExpr] -> IO (Env SExpr, [SExpr])
expand_macros context sexprs = do
  let context' = collect_macros context sexprs
  sexprs' <- apply_macros context' $ filter (not . is_defmacro) sexprs
  return (context', sexprs')
  where is_defmacro :: SExpr -> Bool
        is_defmacro (SList (SSymbol "defmacro":_)) = True
        is_defmacro _                              = False

-- | looks through a lexical scope and evaluates all defmacros
collect_macros :: Env SExpr -> [SExpr] -> Env SExpr
collect_macros env sexprs = Env.append add_lexical' env
  where add_lexical = foldl (\acc sexpr
                              -> case sexpr of
                                   SList (SSymbol "defmacro":definition)
                                     -> let (name, macro) = handle_defmacro env definition
                                        in   Map.insert name (SCallable macro) acc
                                   _ -> acc)
                      Map.empty
                      sexprs
        add_lexical' = fmap (\(SCallable (Macro local_env prototype sexprs bound)) ->
                               SCallable $ Macro (Env.append add_lexical' local_env) prototype sexprs bound) add_lexical

data State = Default | Quote | Backquote

-- | applies macros in a certain lexical scope
-- | all macros are stored in Env SExpr
apply_macros :: Env SExpr -> [SExpr] -> IO [SExpr]
apply_macros env sexprs = mapM (apply_macro Default env) sexprs
  where apply_macro :: State -> Env SExpr -> SExpr -> IO SExpr
        apply_macro Default env (SList (SSymbol "backquote":body)) = do
          body' <- mapM (apply_macro Backquote env) body
          return $ SList (SSymbol "backquote" : body')

        apply_macro Default env (SList (SSymbol "quote":body)) = do
          body' <- mapM (apply_macro Quote env) body
          return $ SList (SSymbol "quote" : body')

        apply_macro Default env list@(SList (SSymbol sym:body)) = case Env.lookup sym env of
          Just (SCallable (Macro local_env prototype sexprs bound)) -> do
            body' <- mapM (apply_macro Default env) body
            let arg_bindings = bind_args prototype (bound ++ body')
            (_, macro_expr) <- eval_scope (Env.append arg_bindings local_env) sexprs
            apply_macro Default env macro_expr
          _                                                         -> do
            list' <- mapM (apply_macro Default env) (from_list list)
            return $ SList list'

        apply_macro Default env      other                      = return other

        apply_macro Quote    _       sexpr                      = return sexpr

        apply_macro Backquote env (SList (SSymbol "interpolate":body)) = do
          body' <- mapM (apply_macro Default env) body
          return $ SList (SSymbol "interpolate" : body')

        apply_macro Backquote env other                                = return other
-- | takes a list of the form (name (arg1 arg2... [&rest argLast]) body...)
-- | and constructs a Macro object (Callable)
-- | also returns its name
handle_defmacro :: Env SExpr -> [SExpr] -> (String, Callable Env SExpr)
handle_defmacro context (s_name:s_lambda_list:body)
  | not $ is_symbol s_name = error "macro name must be a symbol"
  | otherwise              = (name, Macro context prototype body [])
  where name      = from_symbol s_name
        prototype = parse_lambda_list s_lambda_list

-- | looks through a lexical scope, executes all defines,
-- | and evaluates the remaining s-expressions
eval_functions :: Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
eval_functions context sexprs = do
  let context' = collect_functions context sexprs
  sexpr' <- apply_functions context' $ filter (not . is_define) sexprs
  return (context', sexpr')
  where is_define :: SExpr -> Bool
        is_define (SList (SSymbol "define":_)) = True
        is_define _                            = False

-- | looks through a lexical scope and evaluates all defines
collect_functions :: Env SExpr -> [SExpr] -> Env SExpr
collect_functions env sexprs = Env.append add_lexical' env
  where add_lexical = foldl (\acc sexpr
                              -> case sexpr of
                                   SList (SSymbol "define":definition)
                                     -> let (name, function) = handle_define env definition
                                        in  Map.insert name (SCallable function) acc
                                   _ -> acc)
                      Map.empty
                      sexprs
        add_lexical' = fmap (\(SCallable (UserDefined local_env prototype sexprs bound)) ->
                               SCallable $ UserDefined (Env.append add_lexical' local_env) prototype sexprs bound)
                       add_lexical

-- | evaluates functions in a certain lexical scope
apply_functions :: Env SExpr -> [SExpr] -> IO SExpr
apply_functions env sexprs = do
  (_, sexpr) <- foldM (\(prev_env, _) sexpr -> eval prev_env sexpr) (env, nil) sexprs
  return sexpr

-- | takes an s-list of the form (name (arg1 arg2... [&rest lastArg]) body...)
-- | and constructs the correspoding UserDefined object (Callable)
-- | also returns its name
handle_define :: Env SExpr -> [SExpr] -> (String, Callable Env SExpr)
handle_define context (s_name:s_lambda_list:body)
  | not $ is_symbol s_name = error "function name must be a symbol"
  | otherwise              = (name, UserDefined context prototype body [])
  where name      = from_symbol s_name
        prototype = parse_lambda_list s_lambda_list

eval :: Env SExpr -> SExpr -> IO (Env SExpr, SExpr)
eval env (SList (first:rest)) = do
  (_, first') <- eval env first
  case first' of
    SCallable (UserDefined local_env prototype sexprs bound) -> do
      pairs <- mapM (eval env) rest
      let arg_bindings = bind_args prototype (bound ++ map snd pairs)
      eval_scope (Env.append arg_bindings local_env) sexprs
    SCallable (BuiltIn _ _ f bound)                          -> do
      pairs <- mapM (eval env) rest
      result <- f (bound ++ map snd pairs)
      return (env, result)
    SCallable (SpecialOp _ _ f bound)                        -> f eval eval_scope env (bound ++ rest)
    _                                                        -> error $ "unable to execute s-expression: '" ++ show_sexpr first' ++ "'"
eval env (SSymbol sym)        = case Env.lookup sym env of
  Just smth -> return (env, smth)
  Nothing   -> error $ "undefined identificator '" ++ sym ++ "'"
eval env sexpr                = return (env, sexpr)

load_prelude :: IO (Env SExpr)
load_prelude = do
  text <- readFile "stdlib/prelude.lisp"
  (env, _) <- eval_scope start_env $ Reader.read Undefined text -- TODO: change Undefined
  return (Env.pass env)

start_env :: Env SExpr
start_env = Env.fromList $
    (fmap (\(name, args, f) -> (name, SCallable $ SpecialOp name args f [])) [
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
  (fmap (\(name, args, f) -> (name, SCallable $ BuiltIn name args f [])) [
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

spop_context_from_file :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_context_from_file eval eval_scope env [arg] = do
  (_, sexpr) <- eval env arg
  case sexpr of
    SString filename -> do
      text <- readFile filename
      env' <- evaluate_module $ Reader.read Undefined text -- TODO: change Undefined
      return (env, SEnv env')
    _                -> error "context-from-file: string expected"
spop_context_from_file _    _          _        _    = error "context-from-file: just one argument required"

spop_context_from_file_no_prelude :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_context_from_file_no_prelude eval eval_scope env [arg] = do
  (_, sexpr) <- eval env arg
  case sexpr of
    SString filename -> do
      text <- readFile filename
      env' <- evaluate_module_no_prelude $ Reader.read Undefined text -- TODO: change Undefined
      return (env, SEnv env')
    _                -> error "context-from-file-no-prelude: string expected"
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
  where ixs   = elemIndices (SSymbol "&rest") lambda_list
        ix    = head ixs
        rest  = length ixs == 1
        count = length lambda_list
parse_lambda_list _ = error "lambda list must be a list"
