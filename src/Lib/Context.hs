{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Context (spop_context,
                    spop_load_context,
                    spop_import_context,
                    spop_current_context,
                    builtin_function_context) where
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Env
import qualified Reader
import Callable
import LexicalEnvironment
import SExpr
import LispShow
import Exception

spop_context :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_context eval eval_scope e args = do
  pairs <- mapM (eval e) args
  let symbols = map snd pairs
  return $ if not $ all is_symbol symbols
           then report_undef "context: symbol expected"
           else (e, env $ extract_symbols e $ map (\s -> (from_symbol s, point s)) symbols)

extract_symbols :: LEnv SExpr -> [(String, Point)] -> Map String SExpr
extract_symbols e keys = foldl (\acc (key, p) -> case Env.lookup key e of
                                   Just value -> Map.insert key value acc
                                   Nothing    -> report p $ "context: undefined symbol '" ++ key ++ "'")
                           Map.empty
                           keys

spop_import_context :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_import_context eval _ e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom _ (AEnv add) -> (Env.xappend e add, nil)
    _                  -> report (point sexpr) "import-context: context expected"
spop_import_context_    _ _ []     = report_undef "import-context: just one argument required"

spop_load_context :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_load_context eval _ e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom _ (AEnv add) -> (Env.lappend e add, nil)
    _                  -> report (point sexpr) "load-context: context expected"
spop_load_context _   _ _ []     = report_undef "load-context: just one argument required"

spop_current_context :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_current_context _ _ e [] = return (e, env $ Env.merge e)
spop_current_context _ _ _ _  = report_undef "current-context: no arguments required"

builtin_function_context :: [SExpr] -> IO SExpr
builtin_function_context [SAtom _ (ACallable (UserDefined e _ _ _))] = return . env $ Env.merge e
builtin_function_context [sexpr]                                     = report (point sexpr) "function-context: function expected"
builtin_function_context _                                           = report_undef "function-context: just one argument required"
