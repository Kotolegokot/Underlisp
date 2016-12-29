{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
module Atom where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Text.Read (readMaybe)
import Callable
import Env
import LispShow

data Atom e a where
  AInt      ::                    Int              -> Atom e a
  AFloat    ::                    Float            -> Atom e a
  AChar     ::                    Char             -> Atom e a
  ABool     ::                    Bool             -> Atom e a
  ASymbol   ::                    String           -> Atom e a
  ACallable :: (Eq a, Env e a) => Callable e a     -> Atom e a
  AEnv      :: Eq a =>            Map String a     -> Atom e a

instance (LispShow a) => LispShow (Atom e a) where
  lispShow (AInt i)      = show i
  lispShow (AFloat f)    = show f
  lispShow (AChar c)     = ['#', c]
  lispShow (ABool b)     = show b
  lispShow (ASymbol s)   = s
  lispShow (ACallable c) = '{' : lispShow c ++ "}"
  lispShow (AEnv e)      = '{' : lispShow e ++ "}"

instance LispShow a => LispShow (Map String a) where
  lispShow map = unlines $ Map.foldMapWithKey (\key value -> [key ++ space key ++ " => " ++ lispShow value]) map
    where indent    = maximum . fmap length $ Map.keys map
          space key = replicate (indent - length key) ' '

instance Eq (Atom e a) where
  (AInt i)      == (AInt i')      = i == i'
  (AFloat f)    == (AFloat f')    = f == f'
  (AChar c)     == (AChar c')     = c == c'
  (ABool b)     == (ABool b')     = b == b'
  (ASymbol s)   == (ASymbol s')   = s == s'
  (ACallable c) == (ACallable c') = undefined
  (AEnv e)      == (AEnv e')      = e == e'
  _             == _              = False

instance Ord (Atom e a) where
  compare (AInt i)      (AInt i')      = compare i i'
  compare (AFloat f)    (AFloat f')    = compare f f'
  compare (AChar c)     (AChar c')     = compare c c'
  compare (ABool b)     (ABool b')     = compare b b'
  compare (ASymbol s)   (ASymbol s')   = compare s s'
  compare (ACallable _) (ACallable _') = undefined
  compare (AEnv e)      (AEnv e')      = undefined
  compare _             _              = undefined

isInt :: Atom e a -> Bool
isInt (AInt _) = True
isInt _        = False

fromInt :: Atom e a -> Int
fromInt (AInt i) = i
fromInt _        = undefined

isFloat :: Atom e a -> Bool
isFloat (AFloat _) = True
isFloat _          = False

fromFloat :: Atom e a -> Float
fromFloat (AFloat f) = f
fromFloat _          = undefined

isNumber :: Atom e a -> Bool
isNumber (AInt _)   = True
isNumber (AFloat _) = True
isNumber _          = False

fromNumber :: Atom e a -> Float
fromNumber (AInt i)   = fromIntegral i
fromNumber (AFloat f) = f
fromNumber _          = undefined

isChar :: Atom e a -> Bool
isChar (AChar _) = True
isChar _         = False

fromChar :: Atom e a -> Char
fromChar (AChar c) = c
fromChar _         = undefined

isBool :: Atom e a -> Bool
isBool (ABool _) = True
isBool _         = False

fromBool :: Atom e a -> Bool
fromBool (ABool b) = b
fromBool _         = undefined

isSymbol :: Atom e a -> Bool
isSymbol (ASymbol _) = True
isSymbol _           = False

fromSymbol :: Atom e a -> String
fromSymbol (ASymbol s) = s
fromSymbol _           = undefined

isCallable :: Atom e a -> Bool
isCallable (ACallable _) = True
isCallable _             = False

fromCallable :: Atom e a -> Callable e a
fromCallable (ACallable c) = c
fromCallable _             = undefined

isEnv :: Atom e a -> Bool
isEnv (AEnv _) = True
isEnv _        = False

fromEnv :: Atom e a -> Map String a
fromEnv (AEnv e) = e
fromEnv _        = undefined

strToAtom :: String -> Atom e a
strToAtom atom
  | isJust tryInt    = AInt     $ fromJust tryInt
  | isJust tryFloat  = AFloat   $ fromJust tryFloat
  | isJust tryChar   = AChar    $ fromJust tryChar
  | isJust tryBool   = ABool    $ fromJust tryBool
  | otherwise        = ASymbol atom
  where tryInt    = readMaybe atom :: Maybe Int
        tryFloat  = readMaybe atom :: Maybe Float
        tryChar   = readMaybe atom :: Maybe Char
        tryBool   = readMaybe atom :: Maybe Bool

atomType :: Atom e a -> String
atomType (AInt _)      = "Int"
atomType (AFloat _)    = "Float"
atomType (AChar _)     = "Char"
atomType (ABool _)     = "Bool"
atomType (ASymbol _)   = "Symbol"
atomType (ACallable _) = "Callable"
atomType (AEnv _)      = "Env"
