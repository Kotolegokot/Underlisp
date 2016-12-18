module Evaluator (evaluate_program, evaluate_module) where

import qualified Data.Set as Set
import qualified Data.Map as Map
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

evaluate_module :: [SExpr] -> IO Context
evaluate_module body = do
  prelude <- load_prelude
  (context, _) <- eval_scope prelude body
  return context

evaluate_module_no_prelude :: [SExpr] -> IO Context
evaluate_module_no_prelude body = do
  (context, _) <- eval_scope start_context body
  return context

-- | evaluates a lexical scope
-- | 1. expands all macros in it
-- | 2. collect all function definitions
-- | 3. executes the remaining s-expressions successively
eval_scope :: Context -> [SExpr] -> IO (Context, SExpr)
eval_scope context sexprs = expand_macros context sexprs >>= (uncurry collect_defines) >>= (uncurry eval_list)
  where eval_list :: Context -> [SExpr] -> IO (Context, SExpr)
        eval_list context = foldM (\(prev_context, _) sexpr -> eval prev_context sexpr) (context, nil)

-- | looks through a lexical scope, executes all defmacros,
-- | and expands them
expand_macros :: Context -> [SExpr] -> IO (Context, [SExpr])
expand_macros context sexprs = apply_macros (collect_macros Map.empty (context, sexprs), filter (not . is_defmacro) sexprs)
  where collect_macros :: Context -> (Context, [SExpr]) -> Context
        collect_macros lexical_scope (context, (SList (SSymbol "defmacro":definition):rest)) =
          let (name, macro) = handle_defmacro context definition
          in  collect_macros (Map.insert name (SCallable macro) lexical_scope) (context, rest)
        collect_macros lexical_scope (context, (_:rest)) = collect_macros lexical_scope (context, rest)
        collect_macros lexical_scope (context, [])       =
          let lexical_scope' = fmap (\(SCallable (Macro context prototype sexprs bound)) ->
                                           SCallable (Macro (lexical_scope' `Map.union` context) prototype sexprs bound)) lexical_scope
          in  lexical_scope' `Map.union` context

        is_defmacro :: SExpr -> Bool
        is_defmacro (SList (SSymbol "defmacro":_)) = True
        is_defmacro _                              = False

-- | applies macros in a certain lexical scope
-- | all macros are stored in Context
apply_macros :: (Context, [SExpr]) -> IO (Context, [SExpr])
apply_macros (context, (x:xs)) = do
  x' <- apply_macro context x
  (_, rest') <- apply_macros (context, xs)
  return (context, x' : rest')
  where apply_macro :: Context -> SExpr -> IO SExpr
        apply_macro context (SList (SSymbol sym:body)) = case Map.lookup sym context of
          Just (SCallable (Macro context prototype sexprs bound)) -> do
            let arg_bindings = bind_args prototype (bound ++ body)
            body' <- mapM (apply_macro context) body
            (_, sexpr) <- eval_scope context sexprs
            return sexpr
          Nothing                                                 -> return (SList $ SSymbol sym : body)
        apply_macro context other                      = return other
apply_maros (context, [])     = return (context, [])

-- | creates argument bindings from a Prototype
-- | and arguments (s-expressions)
bind_args :: Prototype -> [SExpr] -> Context
bind_args (Prototype arg_names False) args
  | length arg_names > length args = error "too little arguments"
  | length arg_names < length args = error "too many arguments"
  | otherwise                      = foldl (\context (name, value) -> Map.insert name value context) Map.empty (zip arg_names args)
bind_args (Prototype arg_names True) args
  | length arg_names - 1 > length args = error "too little arguments"
  | otherwise                          = let (left, right) = splitAt (length arg_names - 1) args
                                             args'         = left ++ [SList right]
                                         in foldl (\context (name, value) -> Map.insert name value context) Map.empty (zip arg_names args')

-- | takes a list of the form (name (arg1 arg2... [&rest argLast]) body...)
-- | and constructs a Macro object (Callable)
-- | also returns its name
handle_defmacro :: Context -> [SExpr] -> (String, Callable)
handle_defmacro context (s_name:s_lambda_list:body)
  | not $ is_symbol s_name = error "macro name must be a symbol"
  | otherwise              = (name, Macro context prototype body [])
  where name      = from_string s_name
        prototype = parse_lambda_list s_lambda_list

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

-- | looks through a lexical scope and evaluate all defines
collect_defines :: Context -> [SExpr] -> IO (Context, [SExpr])
collect_defines context sexprs = do
  context' <- collect_defines' Map.empty context sexprs
  return (context', filter (not . is_define) sexprs)
    where collect_defines' :: Context -> Context -> [SExpr] -> IO Context
          collect_defines' lexical_scope context (SList (SSymbol "define":definition):rest) =
            let (name, function) = handle_define context definition
            in  collect_defines' (Map.insert name (SCallable function) lexical_scope) context rest
          collect_defines' lexical_scope context (_:rest) = collect_defines' lexical_scope context rest
          collect_defines' lexical_scope context []       = return $
            let lexical_scope' = fmap (\(SCallable (UserDefined context prototype sexprs bound)) ->
                                         SCallable (UserDefined (lexical_scope' `Map.union` context) prototype sexprs bound)) lexical_scope
            in  lexical_scope' `Map.union` context

          is_define :: SExpr -> Bool
          is_define (SList (SSymbol "define":_)) = True
          is_define _                            = False

-- | takes an s-list of the form (name (arg1 arg2... [&rest lastArg]) body...)
-- | and constructs the correspoding UserDefined object (Callable)
-- | also returns its name
handle_define :: Context -> [SExpr] -> (String, Callable)
handle_define context (s_name:s_lambda_list:body)
  | not $ is_symbol s_name = error "function name must be a symbol"
  | otherwise              = (name, UserDefined context prototype body [])
  where name      = from_string s_name
        prototype = parse_lambda_list s_lambda_list

eval :: Context -> SExpr -> IO (Context, SExpr)
eval context (SList (first:rest)) = do
  (_, first') <- eval context first
  case first' of
    SCallable (UserDefined l_context prototype sexprs bound) -> do
      pairs <- mapM (eval context) rest
      let arg_bindings = bind_args prototype (bound ++ map snd pairs)
      eval_scope (arg_bindings `Map.union`  l_context) sexprs
    SCallable (BuiltIn _ _ f bound)                          -> do
      pairs <- mapM (eval context) rest
      result <- f (bound ++ map snd pairs)
      return (context, result)
    SCallable (SpecialOp _ _ f bound)                        -> f eval eval_scope context (bound ++ rest) :: IO (Context, SExpr)
    _                                                        -> error $ "unable to execute s-expression: '" ++ show_sexpr first' ++ "'"
eval context (SList [])           = error "unable to execute an empty list"
eval context (SSymbol sym)        = case Map.lookup sym context of
  Just smth -> return (context, smth)
  Nothing   -> error $ "undefined identificator '" ++ sym ++ "'"
eval context sexpr                = return (context, sexpr)

load_prelude :: IO Context
load_prelude = return start_context
{--load_prelude = do
  text <- readFile "stdlib/prelude.lisp"
  (_, context) <- eval_list_with_context eval_sexpr start_context $ Reader.read Undefined text -- TODO: change Undefined
  return context--}

start_context :: Context
start_context = Map.fromList $
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

spop_context_from_file :: Eval -> EvalScope -> Context -> [SExpr] -> IO (Context, SExpr)
spop_context_from_file eval eval_scope context [arg] = do
  (_, sexpr) <- eval context arg
  case sexpr of
    SString filename -> do
      text <- readFile filename
      context' <- evaluate_module $ Reader.read Undefined text -- TODO: change Undefined
      return (context, SContext context')
    _                -> error "context-from-file: string expected"
spop_context_from_file _    _          _        _    = error "context-from-file: just one argument required"

spop_context_from_file_no_prelude :: Eval -> EvalScope -> Context -> [SExpr] -> IO (Context, SExpr)
spop_context_from_file_no_prelude eval eval_scope context [args] = do
  (_, sexpr) <- eval context args
  case sexpr of
    SString filename -> do
      text <- readFile filename
      context' <- evaluate_module_no_prelude $ Reader.read Undefined text -- TODO: change Undefined
      return (context, SContext context')
    _                -> error "context-from-file-no-prelude: string expected"
spop_context_from_file_no_prelude _    _          _       _      = error "context-from-file-no-prelude: just one argument required"
