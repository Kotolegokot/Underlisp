module Evaluator (evaluate_program, evaluate_module) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Reader
import Control.Arrow
import Control.Monad (void, foldM)
import SExpr
import Util
import Lib.Everything

evaluate_program :: [SExpr] -> IO ()
evaluate_program body = do
  prelude <- load_prelude
  void $ eval_list eval_sexpr prelude body

evaluate_module :: [SExpr] -> IO Context
evaluate_module body = do
  prelude <- load_prelude
  (_, context) <- foldM (\(_, prev_context) sexpr -> eval_sexpr prev_context sexpr) (nil, prelude) body
  return context

evaluate_module_no_prelude :: [SExpr] -> IO Context
evaluate_module_no_prelude body = do
  (_, context) <- foldM (\(_, prev_context) sexpr -> eval_sexpr prev_context sexpr) (nil, start_context) body
  return context

eval_sexpr :: Context -> SExpr -> IO (SExpr, Context)
eval_sexpr context (SList (first:args)) = do
  (expr, _) <- eval_sexpr context first
  case expr of
    SCallable (UserDefined l_context arg_names rest sexpr bound) -> do
        pairs <- mapM (eval_sexpr context) args
        let f_context = handle_args arg_names rest (bound ++ fmap fst pairs)
        eval_sexpr (f_context `Map.union` l_context) sexpr
    SCallable (Macro l_context arg_names rest sexpr bound) -> do
        let f_context = handle_args arg_names rest (bound ++ args)
        (expr, _) <- eval_sexpr (f_context `Map.union` (l_context `Map.union` context)) sexpr
        eval_sexpr context expr
    SCallable (BuiltIn _ _ f bound)                       -> do
        pairs <- mapM (eval_sexpr context) args
        result <- f (bound ++ (fmap fst pairs))
        return (result, context)
    SCallable (SpecialOp _ _ f bound)                     -> f eval_sexpr context (bound ++ args)
    _                                         -> error $ "can't execute s-expression: '" ++ show_sexpr expr ++ "'"
eval_sexpr context (SList [])           = error "can't execute empty list"
eval_sexpr context (SSymbol str) 
  | str `Map.member` context = return (context Map.! str, context)
  | otherwise                = error $ "undefined identificator '" ++ str ++ "'"
eval_sexpr context sexpr                = return (sexpr, context)

handle_args :: [String] -> Bool -> [SExpr] -> Context
handle_args arg_names False args
  | length arg_names > length args = error "too little arguments"
  | length arg_names < length args = error "too many arguements"
  | otherwise                    = foldl (\context (name, value) -> Map.insert name value context) Map.empty (zip arg_names args)
handle_args arg_names True args
  | length arg_names - 1 > length args = error "too little arguments"
  | otherwise                          = let (left, right) = splitAt (length arg_names - 1) args
                                          in let args' = left ++ [SList right]
                                              in foldl (\context (name, value) -> Map.insert name value context) Map.empty (zip arg_names args')

eval_scope :: (Context, [SExpr]) -> IO (Context, SExpr)
eval_scope = expand_macros >>= handle_defines >>= eval_list
  where eval_list :: (Context, [SExpr]) -> IO (Context, SExpr)
        eval_list (context, (x:xs)) = eval (context, x) >> eval_rest (context, xs)
        eval_list (context, [x])    = (context, eval (context, x))
        eval_list (context, [])     = (context, nil)

expand_macros :: (Context, [SExpr]) -> IO (Context, [SExpr])
expand_macros (context, sexprs) = apply_macros (collect_macros Map.empty, filter (not . is_defmacro) sexprs)
  where collect_macros :: Context -> (Context, [SExpr]) -> Context
        collect_macros lexical_scope (context, (SList [SSymbol "defmacro":definition]:rest)) =
          let (name, value) = handle_macro definition
          in  collect_macros (Map.insert name value lexical_scope) (context, rest)
        collect_macros lexical_scope (context, (_:rest)) = collect_macros lexical_scope (context, rest)
        collect_macros lexical_scope (context, [])       =
          let new_lexical_scope = fmap (\(Macro context prototype sexprs bound) ->
                                           Macro (new_lexical_scope `Map.union` context) prototype sexprs bound) lexical_scope
          in  new_lexical_scope `Map.union` context

        is_defmacro :: SExpr -> Bool
        is_defmacro (SList (SSymbol "defmacro":_)) = True
        is_defmacro                                = False

        apply_macros :: (Context, [SExpr]) -> IO (Context, [SExpr])
        apply_macros (context, (x:xs)) = do
          x' <- apply_macro context x
          rest' <- apply_macros (context, xs)
          return (context, x' : rest')
        apply_macros (context, [])     = return (context, [])

        apply_macro :: Context -> SExpr -> IO SExpr
        apply_macro (context, SList (SSymbol sym:body)) = case Map.lookup sym context of
          Just (SCallable (Macro context prototype sexprs bound)) -> do
            let arg_bindings = handle_args prototype (bound ++ body)
            body' <- mapM (apply_macro context) body
            (_, sexpr) <- eval_scope (context, sexprs)
            return sexpr
          Nothing                                                 -> return (SList $ SSymbol sym : body)
        apply_macro (context, other)                    = return other

handle_macro :: Context -> [SExpr] -> Callable
handle_macro (s_name:s_lambda_list:body)
  | not $ is_symbol s_name = error "macro name must be a symbol"
  | otherwise              = Macro name Map.empty prototype body []
  where name = from_string s_name
        prototype = Evaluator.handle_lambda_list s_lambda_list -- TODO remove "Evaluator."

handle_lambda_list :: SExpr -> Prototype
handle_lambda_list (SList lambda_list)
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
handle_lmabda_list _ = error "lambda list must be a list"

handle_defines :: (Context, [SExpr]) -> IO (Context, [SExpr])
eval :: (Context, SExpr) -> IO SExpr

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
    ("lambda",                       Nothing, spop_lambda),
    ("defvar",                       Just 2,  spop_defvar),
    ("if",                           Just 3,  spop_if),
    ("macro",                        Nothing, spop_macro),
    ("macro-expand",                 Just 1,  spop_macro_expand),
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

spop_context_from_file :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_context_from_file eval context [args] = do
  (sexpr, _) <- eval context args
  case sexpr of
    SString filename -> do
      text <- readFile filename
      new_context <- evaluate_module $ Reader.read Undefined text -- TODO: change Undefined
      return (SContext new_context, context)
    _                -> error "context-from-file: string expected"
spop_context_from_file _    _       _      = error "context-from-file: just one argument required"

spop_context_from_file_no_prelude :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_context_from_file_no_prelude eval context [args] = do
  (sexpr, _) <- eval context args
  case sexpr of
    SString filename -> do
      text <- readFile filename
      new_context <- evaluate_module_no_prelude $ Reader.read Undefined text -- TODO: change Undefined
      return (SContext new_context, context)
    _                -> error "context-from-file-no-prelude: string expected"
