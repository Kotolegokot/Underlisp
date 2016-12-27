{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes         #-}
module Callable (Callable (..)
                , bind
                , Eval
                , EvalScope
                , module Prototype) where
import LispShow
import Env
import Prototype
import Exception

type Eval      e a = Env e a => e a -> a   -> IO (e a, a)
type EvalScope e a = Env e a => e a -> [a] -> IO (e a, a)

-- Callable
data Callable e a where
  -- lexical scope -> prototype -> s-expressions -> bound args
  UserDefined :: (Env e a) => e a -> Prototype -> [a] -> [a] -> Callable e a
  -- lexical scope -> prototype -> s-expressions -> bound args
  Macro       :: Env e a => e a -> Prototype -> [a] -> [a] -> Callable e a
  -- name -> args count or rest -> function -> bound args
  BuiltIn     :: String -> Maybe Int -> ([a] -> IO a) -> [a] -> Callable e a
  -- name -> args count or rest -> function -> bound args
  SpecialOp   :: Env e a => String -> Maybe Int -> (Eval e a -> EvalScope e a -> e a -> [a] -> IO (e a, a)) -> [a] -> Callable e a

instance LispShow a => LispShow (Callable e a) where
  lisp_show (UserDefined _ prototype _ bound) = "user-defined function " ++ lisp_show prototype ++ " " ++ lisp_show bound
  lisp_show (Macro       _ prototype _ bound) = "macro " ++ lisp_show prototype ++ " " ++ lisp_show bound
  lisp_show (BuiltIn   name _ _ bound)        = "built-in function '" ++ name ++ "' " ++ lisp_show bound
  lisp_show (SpecialOp name _ _ bound)        = "special operator '" ++ name ++ "' " ++ lisp_show bound

-- bind --
bind :: Callable e a -> [a] -> Callable e a

bind (UserDefined scope prototype@(Prototype arg_names rest) sexprs bound) args
  | rest && length arg_names < (length bound + length args) = report_undef "too many arguments"
  | otherwise                                               = UserDefined scope prototype sexprs (bound ++ args)

bind (Macro scope prototype@(Prototype arg_names rest) sexprs bound) args
  | rest && length arg_names < (length bound + length args) = report_undef "too many arguments"
  | otherwise                                               = Macro scope prototype sexprs (bound ++ args)

bind (BuiltIn name (Just args_count) f bound) args
  | args_count < (length bound + length args) = report_undef "too many arguments"
  | otherwise                                 = BuiltIn name (Just args_count) f (bound ++ args)
bind (BuiltIn name Nothing f bound) args = BuiltIn name Nothing f (bound ++ args)

bind (SpecialOp name (Just args_count) f bound) args
  | args_count < (length bound + length args) = report_undef "too many arguments"
  | otherwise                                 = SpecialOp name (Just args_count) f (bound ++ args)
bind (SpecialOp name Nothing f bound) args = SpecialOp name Nothing f (bound ++ args)
-- bind --
