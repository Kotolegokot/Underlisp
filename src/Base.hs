{-# LANGUAGE LambdaCase #-}
module Base where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Control.Conditional as C
import Control.Monad.Writer
import Control.Monad.Except
import System.IO (hPrint, stderr)
import Data.Maybe
import Text.Read (readMaybe)

import Fail
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

isNil, isInt, isFloat, isChar, isBool, isSymbol, isProcedure, isEnv, isNumber, isString :: SExpr -> Bool

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

isProcedure (SAtom _ (AProcedure _)) = True
isProcedure _                       = False

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

fromProcedure :: SExpr -> Procedure
fromProcedure (SAtom _ (AProcedure c)) = c
fromProcedure _                       = undefined

fromEnv :: SExpr -> Map String EnvItem
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

procedure :: Procedure -> SExpr
procedure = atom . AProcedure

env :: Map String EnvItem -> SExpr
env = atom . AEnv
---- s-expression ----

---- atom ----
data Atom = ANil
          | AInt      Int
          | AFloat    Float
          | AChar     Char
          | ABool     Bool
          | ASymbol   String
          | AProcedure Procedure
          | AEnv      (Map String EnvItem)

instance Eq Atom where
  ANil          == ANil          = True
  (AInt i)      == (AInt i')     = i == i'
  (AFloat f)    == (AFloat f')   = f == f'
  (AInt i)      == (AFloat f)    = fromIntegral i == f
  (AFloat f)    == (AInt i)      = f == fromIntegral i
  (AChar c)     == (AChar c')    = c == c'
  (ABool b)     == (ABool b')    = b == b'
  (ASymbol s)   == (ASymbol s')  = s == s'
  (AProcedure _) == (AProcedure _) = undefined
  (AEnv e)      == (AEnv e')     = e == e'
  _             == _             = False

instance Ord Atom where
  compare ANil          ANil             = EQ
  compare (AInt i)      (AInt i')        = compare i i'
  compare (AFloat f)    (AFloat f')      = compare f f'
  compare (AInt i)      (AFloat f)       = compare (fromIntegral i) f
  compare (AFloat f)    (AInt i)         = compare f (fromIntegral i)
  compare (AChar c)     (AChar c')       = compare c c'
  compare (ABool b)     (ABool b')       = compare b b'
  compare (ASymbol s)   (ASymbol s')     = compare s s'
  compare (AProcedure _) (AProcedure _') = undefined
  compare (AEnv e)      (AEnv e')        = undefined
  compare _             _                = undefined

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
  show (AProcedure c) = show c
  show (AEnv e)      = show e

instance Type Atom where
  showType a = case a of
    ANil        -> "Nil"
    AInt      _ -> "Int"
    AFloat    _ -> "Float"
    AChar     _ -> "Char"
    ABool     _ -> "Bool"
    ASymbol   _ -> "Symbol"
    AProcedure _ -> "Procedure"
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

---- macro ----
data Macro = Macro Point Env Prototype [SExpr] [SExpr]

instance Eq Macro where
  (==) = undefined

instance Show Macro where
  show = const "#<macro>"
---- macro ----

---- procedure ----
data Procedure = UserDefined Env Prototype [SExpr] [SExpr]
               | BuiltIn     String (Maybe Int) ([SExpr] -> Eval SExpr) [SExpr]
               | SpecialOp   String (Maybe Int) (Env -> [SExpr] -> Eval (Env, SExpr)) [SExpr]

isUserDefined, isBuiltIn, isSpecialOp :: Procedure -> Bool

isUserDefined (UserDefined _ _ _ _) = True
isUserDefined _                     = False

isBuiltIn (BuiltIn _ _ _ _) = True
isBuiltIn _                 = False

isSpecialOp (SpecialOp _ _ _ _) = True
isSpecialOp _                   = False

instance Show Procedure where
  show (UserDefined _ _ _ _)  = "#<procedure>"
  show (BuiltIn name _ _ _)   = "#<procedure:" ++ name ++ ">"
  show (SpecialOp name _ _ _) = "#<special operator:" ++ name ++ ">"
---- procedure ----

---- environment ----
data Env = Env Int [String] [Map String EnvItem]

data EnvItem = EnvSExpr SExpr | EnvMacro Macro
  deriving Eq

instance Show EnvItem where
  show (EnvSExpr sexpr) = show sexpr
  show (EnvMacro macro) = show macro

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

envFromList :: [(String, EnvItem)] -> Env
envFromList l = Env 0 [] [Map.fromList l]

pass :: Env -> Env
pass (Env g args xs) = Env g args (Map.empty : xs)

envLookup :: String -> Env -> Maybe EnvItem
envLookup key (Env g args (x:xs)) = case Map.lookup key x of
  Just value -> Just value
  _          -> envLookup key (Env g args xs)
envLookup _   (Env _ _    [])     = Nothing

lookupMacro :: String -> Env -> Maybe Macro
lookupMacro key (Env g args (x:xs)) = case Map.lookup key x of
  Just (EnvMacro m) -> Just m
  _                 -> lookupMacro key (Env g args xs)
lookupMacro _   (Env _ _    [])     = Nothing

lookupSExpr :: String -> Env -> Maybe SExpr
lookupSExpr key (Env g args (x:xs)) = case Map.lookup key x of
  Just (EnvSExpr s) -> Just s
  _                 -> lookupSExpr key (Env g args xs)
lookupSExpr _   (Env _ _    [])     = Nothing

memberMacro :: String -> Env -> Bool
memberMacro key e = isJust $ lookupMacro key e

memberSExpr :: String -> Env -> Bool
memberSExpr key e = isJust $ lookupSExpr key e

envMember :: String -> Env -> Bool
envMember key (Env _ _ xs) = key `Map.member` Map.unions xs

envDelete :: String -> Env -> Env
envDelete key (Env g args xs) = Env g args $ map (Map.delete key) xs

envMerge :: Env -> Map String EnvItem
envMerge (Env _ _ xs) = Map.unions xs

linsert :: String -> EnvItem -> Env -> Env
linsert key value (Env g args (x:xs)) = Env g args (x' : xs)
  where x' = fmap (\case
                      EnvMacro (Macro p e prototype sexprs bound)
                        -> EnvMacro $ Macro p (linsert key value e) prototype sexprs bound
                      EnvSExpr (SAtom p (AProcedure (UserDefined e prototype sexprs bound)))
                        -> EnvSExpr . SAtom p . AProcedure $ UserDefined (linsert key value e) prototype sexprs bound
                      EnvSExpr other
                        -> EnvSExpr other)
             (Map.insert key value x)
linsert _  _      _                   = undefined

xinsert :: String -> EnvItem -> Env -> Env
xinsert key value (Env g args (x:xs)) = Env g args (x' : ext : xs)
  where ext = Map.fromList [(key, value)]
        x' = fmap (\case
                      EnvMacro (Macro p e prototype sexprs bound)
                        -> EnvMacro $ Macro p (xinsert key value e) prototype sexprs bound
                      EnvSExpr (SAtom p (AProcedure (UserDefined e prototype sexprs bound)))
                        -> EnvSExpr . SAtom p . AProcedure $ UserDefined (xinsert key value e) prototype sexprs bound
                      EnvSExpr other
                        -> EnvSExpr other)
             x
xinsert _   _     _                   = undefined

lappend :: Env -> Map String EnvItem -> Env
lappend (Env g args (x:xs)) add = Env g args (x' : xs)
  where x' = fmap (\case
                      EnvMacro (Macro p e prototype sexprs bound)
                        -> EnvMacro $ Macro p (lappend e add) prototype sexprs bound
                      EnvSExpr (SAtom p (AProcedure (UserDefined e prototype sexprs bound)))
                        -> EnvSExpr . SAtom p . AProcedure $ UserDefined (lappend e add) prototype sexprs bound
                      EnvSExpr other
                        -> EnvSExpr other)
             (add `Map.union` x)
lappend _                   _   = undefined

xappend :: Env -> Map String EnvItem -> Env
xappend (Env g args (x:xs)) add = Env g args (x' : add : xs)
  where x' = fmap (\case
                      EnvMacro (Macro p e prototype sexprs bound)
                        -> EnvMacro $ Macro p (xappend e add) prototype sexprs bound
                      EnvSExpr (SAtom p (AProcedure (UserDefined e prototype sexprs bound)))
                        -> EnvSExpr . SAtom p . AProcedure $ UserDefined (xappend e add) prototype sexprs bound
                      EnvSExpr other
                        -> EnvSExpr other)
             x
xappend _                   _   = undefined

lexical  (Env _ _ (x:_ )) = x
external (Env _ _ (_:xs)) = Map.unions xs
---- environment ----

---- eval ---
data Call = Call { cPoint  :: Point
                 , cExpr   :: SExpr }

type Eval = ExceptT Fail (WriterT [Call] IO)

runEval :: Eval a -> IO (Either Fail a, [Call])
runEval = runWriterT . runExceptT

evalEval :: Eval a -> IO (Either Fail a)
evalEval = runEval >=> return . fst

execEval :: Eval a -> IO [Call]
execEval = runEval >=> return . snd

handleEval :: Eval a -> IO ()
handleEval ev = do
  (result, callstack) <- runEval ev
  case result of
    Right _ -> return ()
    Left f  -> hPrint stderr f -- TODO: print call stack

instance Show Fail where
  show (Fail Undefined msg)                   = msg
  show (Fail (Point filename row column) msg) =
    filename ++ ":" ++ show row ++ ":" ++ show column ++ ": " ++ msg
---- eval ----
