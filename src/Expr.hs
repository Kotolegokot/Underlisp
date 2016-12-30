{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
module Expr where

import qualified Atom as A
import Atom
import Callable
import Data.Map (Map)
import qualified Data.Map as Map
import Env

class Env e a => Expr e a | a -> e where
  isList   :: a -> Bool
  isList = not . isAtom

  isAtom   :: a -> Bool
  isAtom = not . isList

  fromList :: a -> [a]
  fromAtom :: a -> Atom e a

  nil       :: a
  nil = list []

  atom      :: Atom e a -> a
  list      :: [a] -> a

  {-# MINIMAL fromList, fromAtom, atom, list, (isList | isAtom) #-}

exprType :: Expr e a => a -> String
exprType x = if isList x
              then "List"
              else atomType $ fromAtom x

int :: Expr e a => Int -> a
int = atom . AInt

float :: Expr e a => Float -> a
float = atom . AFloat

char :: Expr e a => Char -> a
char = atom . AChar

bool :: Expr e a => Bool -> a
bool  = atom . ABool

symbol :: Expr e a => String -> a
symbol = atom . ASymbol

callable :: (Env e a, Expr e a, Eq a) => Callable e a -> a
callable = atom . ACallable

env :: (Expr e a, Eq a) => Map String a -> a
env = atom . AEnv

(.&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p1 .&& p2 = \a -> p1 a && p2 a

infixr 8 .&&

isInt :: Expr e a => a -> Bool
isInt = isAtom .&& A.isInt . fromAtom

fromInt :: Expr e a => a -> Int
fromInt = A.fromInt . fromAtom

isFloat :: Expr e a => a -> Bool
isFloat = isAtom .&& A.isFloat . fromAtom

fromFloat :: Expr e a => a -> Float
fromFloat = A.fromFloat . fromAtom

isNumber :: Expr e a => a -> Bool
isNumber = isAtom .&& A.isNumber . fromAtom

fromNumber :: Expr e a => a -> Float
fromNumber = A.fromNumber . fromAtom

isChar :: Expr e a => a -> Bool
isChar = isAtom .&& A.isChar . fromAtom

fromChar :: Expr e a => a -> Char
fromChar = A.fromChar . fromAtom

isBool :: Expr e a => a -> Bool
isBool = isAtom .&& A.isBool . fromAtom

fromBool :: Expr e a => a -> Bool
fromBool = A.fromBool . fromAtom

isSymbol :: Expr e a => a -> Bool
isSymbol = isAtom .&& A.isSymbol . fromAtom

fromSymbol :: Expr e a => a -> String
fromSymbol = A.fromSymbol . fromAtom

isCallable :: Expr e a => a -> Bool
isCallable = isAtom .&& A.isCallable . fromAtom

fromCallable :: Expr e a => a -> Callable e a
fromCallable = A.fromCallable . fromAtom

isEnv :: Expr e a => a -> Bool
isEnv = isAtom .&& A.isEnv . fromAtom

fromEnv :: Expr e a => a -> Map String a
fromEnv = A.fromEnv . fromAtom

isString :: Expr e a => a -> Bool
isString = isList .&& (all Expr.isChar . Expr.fromList)

fromString :: Expr e a => a -> String
fromString = map Expr.fromChar . Expr.fromList

toString :: Expr e a => String -> a
toString = list . map char
