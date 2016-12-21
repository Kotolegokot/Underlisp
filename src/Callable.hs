{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
module Callable where

import LispShow
import Env

type Eval      a = Env a -> a   -> IO (Env a, a)
type EvalScope a = Env a -> [a] -> IO (Env a, a)

-- Callable
data Callable a where
  -- lexical scope -> prototype -> s-expressions -> bound args
  UserDefined :: Env a -> Prototype -> [a] -> [a] -> Callable a
  -- lexical scope -> prototype -> s-expressions -> bound args
  Macro       :: Env a -> Prototype -> [a] -> [a] -> Callable a
  -- name -> args count or rest -> function -> bound args
  BuiltIn     :: String -> Maybe Int -> ([a] -> IO a) -> [a] -> Callable a
  -- name -> args count or rest -> function -> bound args
  SpecialOp   :: String -> Maybe Int -> (Eval a -> EvalScope a -> Env a -> [a] -> IO (Env a, a)) -> [a] -> Callable a

instance LispShow a => LispShow (Callable a) where
  lisp_show (UserDefined _ prototype _ bound) = "user-defined function " ++ lisp_show prototype ++ " " ++ lisp_show bound
  lisp_show (Macro       _ prototype _ bound) = "macro " ++ lisp_show prototype ++ " " ++ lisp_show bound
  lisp_show (BuiltIn   name _ _ bound)        = "built-in function '" ++ name ++ "' " ++ lisp_show bound
  lisp_show (SpecialOp name _ _ bound)        = "special operator '" ++ name ++ "' " ++ lisp_show bound

-- bind --
bind :: Callable a -> [a] -> Callable a

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
-- bind --

-- Prototype
data Prototype = Prototype [String] Bool
  deriving (Eq, Show)

instance LispShow Prototype where
  lisp_show (Prototype args rest)
    | rest      = "(" ++ show_list args ++ ")"
    | otherwise = "(" ++ show_list args ++ ")"
    where show_list [x]    = if rest then x else "&rest " ++ x
          show_list (x:xs) = x ++ " " ++ show_list xs
          show_list []     = []
