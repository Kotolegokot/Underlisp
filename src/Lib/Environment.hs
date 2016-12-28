{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Lib.Environment (spop_env,
                        spop_load_env,
                        spop_import_env,
                        spop_current_env,
                        builtin_function_env) where
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Env
import qualified Reader
import Callable
import LexicalEnvironment
import SExpr
import LispShow
import Exception

spop_env :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_env eval eval_scope e args = do
  pairs <- mapM (eval e) args
  let symbols = map snd pairs
  return $ if not $ all is_symbol symbols
           then report_undef "symbol expected"
           else (e, env $ extract_env e $ map (\s -> (from_symbol s, point s)) symbols)

extract_env :: LEnv SExpr -> [(String, Point)] -> Map String SExpr
extract_env e keys = foldl (\acc (key, p) -> case Env.lookup key e of
                               Just value -> Map.insert key value acc
                               Nothing    -> report p $ "undefined symbol '" ++ key ++ "'")
                     Map.empty
                     keys

spop_import_env :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_import_env eval _ e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom _ (AEnv add) -> (Env.xappend e add, nil)
    _                  -> report (point sexpr) "context expected"
spop_import_env_    _ _ []     = report_undef "just one argument required"

spop_load_env :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_load_env eval _ e [arg] = do
  (_, sexpr) <- eval e arg
  return $ case sexpr of
    SAtom _ (AEnv add) -> (Env.lappend e add, nil)
    _                  -> report (point sexpr) "context expected"
spop_load_env _   _ _ []     = report_undef "just one argument required"

spop_current_env :: Eval LEnv SExpr -> EvalScope LEnv SExpr -> LEnv SExpr -> [SExpr] -> IO (LEnv SExpr, SExpr)
spop_current_env _ _ e [] = return (e, env $ Env.merge e)
spop_current_env _ _ _ _  = report_undef "no arguments required"

builtin_function_env :: [SExpr] -> IO SExpr
builtin_function_env [SAtom _ (ACallable (UserDefined e _ _ _))] = return . env $ Env.merge e
builtin_function_env [sexpr]                                     = report (point sexpr) "function expected"
builtin_function_env _                                           = report_undef "just one argument required"
