module Base where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Conditional as C
import Data.Maybe
import Text.Read (readMaybe)

import Exception
import Prototype
import Point
import Type

---- s-expression ----
data SExpr = SList Point [SExpr] | SAtom Point Atom

isList :: SExpr -> Bool
isList (SList _ _) = True
isList _           = False

isEmptyList :: SExpr -> Bool
isEmptyList (SList _ []) = True
isEmptyList _            = False

isAtom :: SExpr -> Bool
isAtom (SAtom _ _) = True
isAtom _           = False

fromList :: SExpr -> [SExpr]
fromList (SList _ xs) = xs
fromList _            = undefined

fromAtom :: SExpr -> Atom
fromAtom (SAtom _ a) = a
fromAtom _           = undefined

nil :: SExpr
nil = SAtom Undefined ANil

atom :: Atom -> SExpr
atom = SAtom Undefined

list :: [SExpr] -> SExpr
list = SList Undefined

point :: SExpr -> Point
point (SList p _) = p
point (SAtom p _) = p

setPoint :: SExpr -> Point -> SExpr
setPoint (SList _ xs) p = SList p xs
setPoint (SAtom _ a)  p = SAtom p a

instance Eq SExpr where
  SList _ s == SList _ s' = s == s'
  SAtom _ a == SAtom _ a' = a == a'
  _         == _          = False

instance Ord SExpr where
  compare (SList _ l) (SList _ l') = compare l l'
  compare (SAtom _ a) (SAtom _ a') = compare a a'
  compare _           _            = undefined

instance Show SExpr where
  show expr
    | isEmptyList expr = "()"
    | isString expr    = show $ fromString expr
    | isList   expr    = "(" ++ showList (fromList expr) ++ ")"
    | isAtom   expr    = show $ fromAtom expr
    where showList [x]    = show x
          showList (x:xs) = show x ++ " " ++ showList xs
          showList []     = ""

instance Type SExpr where
  showType sexpr
    | isList sexpr = "List"
    | isAtom sexpr = showType $ fromAtom sexpr

isNil, isInt, isFloat, isChar, isBool, isSymbol, isCallable, isEnv, isNumber, isString :: SExpr -> Bool

isNil (SAtom _ ANil) = True
isNil _              = False

isInt (SAtom _ (AInt _)) = True
isInt _                  = False

isFloat (SAtom _ (AFloat _)) = True
isFloat _                    = False

isChar (SAtom _ (AChar _)) = True
isChar _                   = False

isBool (SAtom _ (ABool _)) = True
isBool _                   = False

isSymbol (SAtom _ (ASymbol _)) = True
isSymbol _                     = False

isCallable (SAtom _ (ACallable _)) = True
isCallable _                       = False

isEnv (SAtom _ (AEnv _)) = True
isEnv _                  = False

isNumber x = isFloat x || isInt x

isString expr = isList expr && all isChar (fromList expr)

fromInt :: SExpr -> Int
fromInt (SAtom _ (AInt i)) = i
fromInt _                  = undefined

fromFloat :: SExpr -> Float
fromFloat (SAtom _ (AFloat f)) = f
fromFloat _                    = undefined

fromChar :: SExpr -> Char
fromChar (SAtom _ (AChar c)) = c
fromChar _                   = undefined

fromBool :: SExpr -> Bool
fromBool (SAtom _ (ABool b)) = b
fromBool _                   = undefined

fromSymbol :: SExpr -> String
fromSymbol (SAtom _ (ASymbol s)) = s
fromSymbol _                     = undefined

fromCallable :: SExpr -> Callable
fromCallable (SAtom _ (ACallable c)) = c
fromCallable _                       = undefined

fromEnv :: SExpr -> Map String SExpr
fromEnv (SAtom _ (AEnv e)) = e
fromEnv _                  = undefined

fromNumber :: SExpr -> Float
fromNumber n
  | isInt   n = fromIntegral $ fromInt n
  | isFloat n = fromFloat n
  | otherwise = undefined

fromString :: SExpr -> String
fromString s = map fromChar $ fromList s

toString :: String -> SExpr
toString = list . map char

int :: Int -> SExpr
int = atom . AInt

float :: Float -> SExpr
float = atom . AFloat

char :: Char -> SExpr
char = atom . AChar

bool :: Bool -> SExpr
bool = atom . ABool

symbol :: String -> SExpr
symbol = atom . ASymbol

callable :: Callable -> SExpr
callable = atom . ACallable

env :: Map String SExpr -> SExpr
env = atom . AEnv
---- s-expression ----

---- atom ----
data Atom = ANil
          | AInt      Int
          | AFloat    Float
          | AChar     Char
          | ABool     Bool
          | ASymbol   String
          | ACallable Callable
          | AEnv      (Map String SExpr)

instance Eq Atom where
  ANil          == ANil          = True
  (AInt i)      == (AInt i')     = i == i'
  (AFloat f)    == (AFloat f')   = f == f'
  (AInt i)      == (AFloat f)    = fromIntegral i == f
  (AFloat f)    == (AInt i)      = f == fromIntegral i
  (AChar c)     == (AChar c')    = c == c'
  (ABool b)     == (ABool b')    = b == b'
  (ASymbol s)   == (ASymbol s')  = s == s'
  (ACallable _) == (ACallable _) = undefined
  (AEnv e)      == (AEnv e')     = e == e'
  _             == _             = False

instance Ord Atom where
  compare ANil          ANil           = EQ
  compare (AInt i)      (AInt i')      = compare i i'
  compare (AFloat f)    (AFloat f')    = compare f f'
  compare (AInt i)      (AFloat f)     = compare (fromIntegral i) f
  compare (AFloat f)    (AInt i)       = compare f (fromIntegral i)
  compare (AChar c)     (AChar c')     = compare c c'
  compare (ABool b)     (ABool b')     = compare b b'
  compare (ASymbol s)   (ASymbol s')   = compare s s'
  compare (ACallable _) (ACallable _') = undefined
  compare (AEnv e)      (AEnv e')      = undefined
  compare _             _              = undefined

instance Show Atom where
  show ANil          = "nil"
  show (AInt i)      = show i
  show (AFloat f)    = show f
  show (AChar c)     = case c of
    '\n' -> "#newline"
    '\t' -> "#tab"
    ' '  -> "#space"
    _    -> ['#', c]
  show (ABool b)     = C.bool "false" "true" b
  show (ASymbol s)   = s
  show (ACallable c) = "{ " ++ show c ++ " }"
  show (AEnv e)      = "{ " ++ show e ++ " }"

instance Type Atom where
  showType a = case a of
    ANil        -> "Nil"
    AInt      _ -> "Int"
    AFloat    _ -> "Float"
    AChar     _ -> "Char"
    ABool     _ -> "Bool"
    ASymbol   _ -> "Symbol"
    ACallable _ -> "Callable"
    AEnv      _ -> "Env"

strToAtom :: String -> Atom
strToAtom atom
  | atom == "true"   = ABool True
  | atom == "false"  = ABool False
  | atom == "nil"    = ANil
  | isJust tryInt    = AInt     $ fromJust tryInt
  | isJust tryFloat  = AFloat   $ fromJust tryFloat
  | otherwise        = ASymbol atom
  where tryInt    = readMaybe atom :: Maybe Int
        tryFloat  = readMaybe atom :: Maybe Float
---- atom ----

---- callable ----
data Callable = UserDefined Env Prototype [SExpr] [SExpr]
              | Macro       Env Prototype [SExpr] [SExpr]
              | BuiltIn     String (Maybe Int) ([SExpr] -> IO SExpr) [SExpr]
              | SpecialOp   String (Maybe Int) (Eval -> EvalScope -> Env -> [SExpr] -> IO (Env, SExpr)) [SExpr]

type Eval      = Env -> SExpr   -> IO (Env, SExpr)
type EvalScope = Env -> [SExpr] -> IO (Env, SExpr)

isUserDefined, isMacro, isBuiltIn, isSpecialOp :: Callable -> Bool

isUserDefined (UserDefined _ _ _ _) = True
isUserDefined _                     = False

isMacro (Macro _ _ _ _) = True
isMacro _               = False

isBuiltIn (BuiltIn _ _ _ _) = True
isBuiltIn _                 = False

isSpecialOp (SpecialOp _ _ _ _) = True
isSpecialOp _                   = False

instance Show Callable where
  show (UserDefined _ prototype _ bound) = "user-defined " ++ show prototype ++ " " ++ show bound
  show (Macro       _ prototype _ bound) = "macro" ++ show prototype ++ " " ++ show bound
  show (BuiltIn   name _ _ bound)        = "built-in '" ++ name ++ "' " ++ show bound
  show (SpecialOp name _ _ bound)        = "special operator '" ++ name ++ "' " ++ show bound

bind :: Callable -> [SExpr] -> Callable
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
---- callable ----

---- environment ----
data Env = Env Int [String] [Map String SExpr]

getG :: Env -> Int
getG (Env g _ _) = g

setG :: Env -> Int -> Env
setG (Env _ args xs) g = Env g args xs

getArgs :: Env -> [String]
getArgs (Env _ args _) = args

setArgs :: Env -> [String] -> Env
setArgs (Env g _ xs) args = Env g args xs

instance Show Env where
  show (Env _ _ xs) = foldr (\(level, map) acc -> acc ++ "level " ++ show level ++ ":\n" ++ show map ++ "\n")
                      ""
                      (zip [1..] xs)

empty :: Env
empty = Env 0 [] [Map.empty]

envFromList :: [(String, SExpr)] -> Env
envFromList l = Env 0 [] [Map.fromList l]

pass :: Env -> Env
pass (Env g args xs) = Env g args (Map.empty : xs)

envLookup :: String -> Env -> Maybe SExpr
envLookup key (Env g args (x:xs)) = case Map.lookup key x of
  Just value -> Just value
  Nothing    -> envLookup key (Env g args xs)
envLookup _   (Env _ _    [])     = Nothing

envMember :: String -> Env -> Bool
envMember key (Env _ _ xs) = key `Map.member` Map.unions xs

envMerge :: Env -> Map String SExpr
envMerge (Env _ _ xs) = Map.unions xs

linsert key value (Env g args (x:xs)) = Env g args (x' : xs)
  where x' = fmap (\sexpr -> if isCallable sexpr
                             then SAtom Undefined (ACallable $ case fromCallable sexpr of
                                                                 UserDefined e prototype sexprs bound
                                                                   -> UserDefined (linsert key value e) prototype sexprs bound
                                                                 other -> other)
                             else sexpr)
             (Map.insert key value x)
linsert _  _      _                   = undefined

xinsert key value (Env g args (x:xs)) = Env g args (x' : ext : xs)
  where ext = Map.fromList [(key, value)]
        x' = fmap (\sexpr -> if isCallable sexpr
                             then callable $ case fromCallable sexpr of
                                    UserDefined e prototype sexprs bound
                                          -> UserDefined (xinsert key value e) prototype sexprs bound
                                    other -> other
                             else sexpr)
             x
xinsert _   _     _                   = undefined

lappend (Env g args (x:xs)) add = Env g args (x' : xs)
  where x' = fmap (\sexpr -> if isCallable sexpr
                             then callable $ case fromCallable sexpr of
                                    UserDefined e prototype sexprs bound
                                          -> UserDefined (lappend e add) prototype sexprs bound
                                    other -> other
                             else sexpr)
             (add `Map.union` x)
lappend _                   _   = undefined

xappend (Env g args (x:xs)) add = Env g args (x' : add : xs)
  where x' = fmap (\sexpr -> if isCallable sexpr
                             then callable $ case fromCallable sexpr of
                                    UserDefined e prototype sexprs bound
                                          -> UserDefined (xappend e add) prototype sexprs bound
                                    other -> other
                             else sexpr)
             x
xappend _                   _   = undefined

lexical  (Env _ _ (x:_ )) = x
external (Env _ _ (_:xs)) = Map.unions xs
---- environment ----
