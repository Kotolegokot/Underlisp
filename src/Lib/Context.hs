module Lib.Context (spop_context,
                    spop_load_context,
                    spop_current_context) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Reader
import Lib.Internal
import SExpr

spop_context :: Eval -> EvalScope -> Context -> [SExpr] -> IO (Context, SExpr)
spop_context eval eval_scope context args = do
  pairs <- mapM (eval context) args
  let symbols = map snd pairs
  return $ if not $ all is_symbol symbols
           then error "context: symbol expected"
           else (context, SContext (extract_symbols context $ map from_symbol symbols))

extract_symbols :: Context -> [String] -> Context
extract_symbols context keys = extract_symbols' keys Map.empty
  where extract_symbols' [] acc = acc
        extract_symbols' (x:xs) acc = case Map.lookup x context of
          Just value -> extract_symbols' xs (Map.insert x value acc)
          Nothing    -> error $ "package: undefined symbol '" ++ x ++ "'"

spop_load_context :: Eval -> EvalScope -> Context -> [SExpr] -> IO (Context, SExpr)
spop_load_context eval eval_scope context [arg] = do
  (_, sexpr) <- eval context arg
  return $ case sexpr of
    SContext add_context -> (add_context `Map.union` context, sexpr)
    _                    -> error "load-context: context expected"
spop_load_context eval eval_scope context []    = error "load-context: just one argument required"

spop_current_context :: Eval -> EvalScope -> Context -> [SExpr] -> IO (Context, SExpr)
spop_current_context _ _ context [] = return (context, SContext context)
spop_current_context _ _ _       _  = error "current-context: no arguments required"

