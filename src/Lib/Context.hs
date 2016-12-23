{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Context (spop_context,
                    spop_load_context,
                    spop_current_context) where
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Env
import qualified Reader
import Callable
import LexicalEnv
import SExpr
import LispShow

spop_context :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_context eval eval_scope e args = do
  pairs <- mapM (eval e) args
  let symbols = map snd pairs
  return $ if not $ all is_symbol symbols
           then error "context: symbol expected"
           else (e, env $ extract_symbols e $ map from_symbol symbols)

extract_symbols :: LEnv SExpr -> [String] -> Map String SExpr
extract_symbols env keys = foldl (\acc key -> case Env.lookup key env of
                                     Just value -> Map.insert key value acc
                                     Nothing    -> error $ "context: undefined symbol '" ++ key ++ "'")
                           Map.empty
                           keys

spop_load_context :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_load_context eval eval_scope e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom (AEnv add) -> (Env.xappend e (lisp_trace_id add), nil)
    _                -> error "load-context: context expected"
spop_load_context eval eval_scope _   []  = error "load-context: just one argument required"

spop_current_context :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_current_context _ _ e [] = return (e, env $ Env.merge e)
spop_current_context _ _ _ _  = error "current-context: no arguments required"
