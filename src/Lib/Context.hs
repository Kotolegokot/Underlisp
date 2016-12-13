module Lib.Context (spop_context,
                    spop_load_context,
                    spop_current_context) where

import qualified Data.Map as Map
import Data.Map (Map)
import Lib.Internal
import SExpr

spop_context :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_context eval context args = do
  pairs <- mapM (eval context) args
  let symbols = map fst pairs
  return $ if not $ all is_symbol symbols
             then error "package: symbol expected"
             else (SContext (extract_symbols context $ map from_symbol symbols), context)

extract_symbols :: Context -> [String] -> Context
extract_symbols context keys = extract_symbols' keys Map.empty
  where extract_symbols' [] acc = acc
        extract_symbols' (x:xs) acc = case Map.lookup x context of
                                        Just value -> extract_symbols' xs (Map.insert x value acc)
                                        Nothing    -> error $ "package: undefined symbol '" ++ x ++ "'"

spop_load_context :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_load_context eval context [arg] = do
  (sexpr, _) <- eval context arg
  return $ case sexpr of
             SContext add_context -> (sexpr, add_context `Map.union` context)
             _                    -> error "load-context: context expected"
spop_load_context eval context []    = error "load-context: just one argument required"

spop_current_context :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
spop_current_context _ context [] = return (SContext context, context)
spop_current_context _ _       _  = error "current-context: no arguments required"
