{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
module Callable (Callable (..)
                , bind
                , Eval
                , EvalScope
                , isUserDefined
                , isMacro
                , isBuiltIn
                , isSpecialOp
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
  UserDefined :: Env e a => e a -> Prototype -> [a] -> [a] -> Callable e a
  -- lexical scope -> prototype -> s-expressions -> bound args
  Macro       :: Env e a => e a -> Prototype -> [a] -> [a] -> Callable e a
  -- name -> args count or rest -> function -> bound args
  BuiltIn     :: String -> Maybe Int -> ([a] -> IO a) -> [a] -> Callable e a
  -- name -> args count or rest -> function -> bound args
  SpecialOp   :: Env e a => String -> Maybe Int -> (Eval e a -> EvalScope e a -> e a -> [a] -> IO (e a, a)) -> [a] -> Callable e a

instance LispShow a => LispShow (Callable e a) where
  lispShow (UserDefined _ prototype _ bound) = "user-defined function " ++ lispShow prototype ++ " " ++ lispShow bound
  lispShow (Macro       _ prototype _ bound) = "macro " ++ lispShow prototype ++ " " ++ lispShow bound
  lispShow (BuiltIn   name _ _ bound)        = "built-in function '" ++ name ++ "' " ++ lispShow bound
  lispShow (SpecialOp name _ _ bound)        = "special operator '" ++ name ++ "' " ++ lispShow bound

isUserDefined, isMacro, isBuiltIn, isSpecialOp :: Callable e a -> Bool

isUserDefined (UserDefined _ _ _ _) = True
isUserDefined _                     = False

isMacro (Macro _ _ _ _) = True
isMacro _               = False

isBuiltIn (BuiltIn _ _ _ _) = True
isBuiltIn _                 = False

isSpecialOp (SpecialOp _ _ _ _) = True
isSpecialOp _                   = False

bind :: Callable e a -> [a] -> Callable e a

bind (UserDefined scope prototype@(Prototype argNames rest) sexprs bound) args
  | rest && length argNames < (length bound + length args) = reportUndef "too many arguments"
  | otherwise                                              = UserDefined scope prototype sexprs (bound ++ args)

bind (Macro scope prototype@(Prototype argNames rest) sexprs bound) args
  | rest && length argNames < (length bound + length args) = reportUndef "too many arguments"
  | otherwise                                              = Macro scope prototype sexprs (bound ++ args)

bind (BuiltIn name (Just argsCount) f bound) args
  | argsCount < (length bound + length args) = reportUndef "too many arguments"
  | otherwise                                 = BuiltIn name (Just argsCount) f (bound ++ args)
bind (BuiltIn name Nothing f bound) args = BuiltIn name Nothing f (bound ++ args)

bind (SpecialOp name (Just argsCount) f bound) args
  | argsCount < (length bound + length args) = reportUndef "too many arguments"
  | otherwise                                 = SpecialOp name (Just argsCount) f (bound ++ args)
bind (SpecialOp name Nothing f bound) args = SpecialOp name Nothing f (bound ++ args)
