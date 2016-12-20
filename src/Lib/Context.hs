module Lib.Context (spop_context,
                    spop_load_context,
                    spop_current_context) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Env
import Env (Env (..))
import qualified Reader
import Lib.Internal
import SExpr

spop_context :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_context eval eval_scope env args = do
  pairs <- mapM (eval env) args
  let symbols = map snd pairs
  return $ if not $ all is_symbol symbols
           then error "context: symbol expected"
           else (env, SEnv (extract_symbols env $ map from_symbol symbols))

extract_symbols :: Env SExpr -> [String] -> Map String SExpr
extract_symbols env keys = foldl (\acc key -> case Env.lookup key env of
                                     Just value -> Map.insert key value acc
                                     Nothing    -> error $ "context: undefined symbol '" ++ key ++ "'")
                           Map.empty
                           keys

spop_load_context :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_load_context eval eval_scope env@(Env lexical external) [arg] = do
  (_, sexpr) <- eval env arg
  return $ case sexpr of
    SEnv add_env -> (new_env, nil)
      where new_lexical  = fmap (\sexpr -> case sexpr of
                                    SCallable (UserDefined local_env prototype sexprs bound)
                                           -> SCallable $ UserDefined (Env.append_ex add_env local_env) prototype sexprs bound
                                    other  -> other)
                           lexical
            new_lexical' = fmap (\sexpr -> case sexpr of
                                    SCallable (UserDefined local_env prototype sexprs bound)
                                           -> SCallable $ UserDefined (Env.append new_lexical' local_env) prototype sexprs bound
                                    other  -> other)
                           new_lexical

            new_env      = Env new_lexical' (add_env `Map.union` external)
    _                    -> error "load-context: context expected"
pop_load_context eval eval_scope _   []    = error "load-context: just one argument required"

spop_current_context :: Eval -> EvalScope -> Env SExpr -> [SExpr] -> IO (Env SExpr, SExpr)
spop_current_context _ _ env [] = return (env, SEnv $ Env.merge env)
spop_current_context _ _ _   _  = error "current-context: no arguments required"

