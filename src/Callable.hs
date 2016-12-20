{-# LANGUAGE GADTs #-}
module Callable where

type Eval      env a = env a -> a   -> IO (env a, a)
type EvalScope env a = env a -> [a] -> IO (env a, a)

data Callable env a where
  -- lexical scope -> arg names -> rest -> s-expressions -> bound args
  UserDefined :: env a -> Prototype -> [a] -> [a] -> Callable env a
  -- lexical scope -> arg names -> rest -> s-expressions -> bound args
  Macro       :: env a -> Prototype -> [a] -> [a] -> Callable env a
  -- name -> args count or rest -> function -> bound args
  BuiltIn     :: String -> Maybe Int -> ([a] -> IO a) -> [a] -> Callable env a
  -- name -> args count or rest -> function -> bound args
  SpecialOp   :: String -> Maybe Int -> (Eval env a -> EvalScope env a -> env a -> [a] -> IO (env a, a)) -> [a] -> Callable env a

instance Show (Callable env a) where
  show _ = "SHOW CALLABLE TODO"

bind :: Callable env a -> [a] -> Callable env a
bind (UserDefined scope prototype@(Prototype arg_names rest) sexprs bound) args
  | rest && length arg_names < (length bound + length args) = error "too many arguments"
  | otherwise                                               = UserDefined scope prototype sexprs (bound ++ args)
bind (Macro scope prototype@(Prototype arg_names rest) sexprs bound) args
  | rest && length arg_names < (length bound + length args) = error "too many arguments"
  | otherwise                                               = Macro scope prototype sexprs (bound ++ args)
bind (BuiltIn name (Just args_count) f bound) args
  | args_count < (length bound + length args) = error "too many arguments"
  | otherwise                                 = BuiltIn name (Just args_count) f (bound ++ args)
bind (BuiltIn name Nothing f bound) args = BuiltIn name Nothing f (bound ++ args)
bind (SpecialOp name (Just args_count) f bound) args
  | args_count < (length bound + length args) = error "too many arguments"
  | otherwise                                 = SpecialOp name (Just args_count) f (bound ++ args)
bind (SpecialOp name Nothing f bound) args = SpecialOp name Nothing f (bound ++ args)

data Prototype = Prototype [String] Bool

instance Show Prototype where
  show _ = "SHOW PROTOTYPE TODO"
--  show (Prototype arg_names rest) =
--    if rest
--    then show_sexpr . SList . map SSymbol $ init arg_names ++ ["&rest", last arg_names]
--    else show_sexpr . SList . map SSymbol $ arg_names
