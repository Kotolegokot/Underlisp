module Base where

-- maps
import qualified Data.Map as Map
import Data.Map (Map)

-- vectors
import qualified Data.Vector as Vec
import Data.Vector (Vector)

-- other
import qualified Control.Conditional as C
import Control.Monad.Except
import Control.Monad.Reader
import Control.Applicative ((<|>))
import System.IO (hPrint, stderr)
import Data.Maybe
import Text.Read (readMaybe)

-- local modules
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
    | isList   expr    = handleList (fromList expr)
    | isAtom   expr    = show $ fromAtom expr
    where handleList [SAtom _ (ASymbol "backquote"),   arg] = "`" ++ show arg
          handleList [SAtom _ (ASymbol "quote"),       arg] = "'" ++ show arg
          handleList [SAtom _ (ASymbol "interpolate"), arg] = "~" ++ show arg
          handleList [SAtom _ (ASymbol "unfold"),      arg] = "@" ++ show arg
          handleList (SAtom _ (ASymbol "bind")   :    args) = "[" ++ showList args ++ "]"
          handleList l@(SAtom _ (ASymbol "list")   :  args)
            | all isChar args = show $ map fromChar args
            | otherwise       = "(" ++ showList l ++ ")"
          handleList l = "(" ++ showList l ++ ")"

          showList [x]    = show x
          showList (x:xs) = show x ++ " " ++ showList xs
          showList []     = ""

instance Type SExpr where
  showType sexpr
    | isList sexpr = "list"
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

isVector (SAtom _ (AVector _)) = True
isVector _                     = False

isProcedure (SAtom _ (AProcedure _)) = True
isProcedure _                       = False

isEnv (SAtom _ (AEnv _)) = True
isEnv _                  = False

isSequence x = isList x || isVector x

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

fromVector :: SExpr -> Vector SExpr
fromVector (SAtom _ (AVector v)) = v
fromVector _                     = undefined

fromProcedure :: SExpr -> Procedure
fromProcedure (SAtom _ (AProcedure c)) = c
fromProcedure _                       = undefined

fromEnv :: SExpr -> Map String EnvItem
fromEnv (SAtom _ (AEnv e)) = e
fromEnv _                  = undefined

fromSequence :: SExpr -> [SExpr]
fromSequence s
  | isList s   = fromList s
  | isVector s = Vec.toList $ fromVector s
  | otherwise  = undefined

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

vector :: Vector SExpr -> SExpr
vector = atom . AVector

procedure :: Procedure -> SExpr
procedure = atom . AProcedure

env :: Map String EnvItem -> SExpr
env = atom . AEnv
---- s-expression ----

---- atom ----
data Atom = ANil
          | AInt       Int
          | AFloat     Float
          | AChar      Char
          | ABool      Bool
          | ASymbol    String
          | AVector    (Vector SExpr)
          | AProcedure Procedure
          | AEnv       (Map String EnvItem)

instance Eq Atom where
  ANil           == ANil           = True
  (AInt i)       == (AInt i')      = i == i'
  (AFloat f)     == (AFloat f')    = f == f'
  (AInt i)       == (AFloat f)     = fromIntegral i == f
  (AFloat f)     == (AInt i)       = f == fromIntegral i
  (AChar c)      == (AChar c')     = c == c'
  (ABool b)      == (ABool b')     = b == b'
  (ASymbol s)    == (ASymbol s')   = s == s'
  (AVector v)    == (AVector v')   = v == v'
  (AProcedure _) == (AProcedure _) = undefined
  (AEnv e)       == (AEnv e')      = e == e'
  _              == _              = False

instance Ord Atom where
  compare ANil           ANil             = EQ
  compare (AInt i)       (AInt i')        = compare i i'
  compare (AFloat f)     (AFloat f')      = compare f f'
  compare (AInt i)       (AFloat f)       = compare (fromIntegral i) f
  compare (AFloat f)     (AInt i)         = compare f (fromIntegral i)
  compare (AChar c)      (AChar c')       = compare c c'
  compare (ABool b)      (ABool b')       = compare b b'
  compare (ASymbol s)    (ASymbol s')     = compare s s'
  compare (AVector v)    (AVector v')     = compare v v'
  compare (AProcedure _) (AProcedure _')  = undefined
  compare (AEnv e)       (AEnv e')        = undefined
  compare _              _                = undefined

instance Show Atom where
  show ANil           = "nil"
  show (AInt i)       = show i
  show (AFloat f)     = show f
  show (AChar c)      = case c of
    '\n' -> "#newline"
    '\t' -> "#tab"
    ' '  -> "#space"
    _    -> ['#', c]
  show (ABool b)      = C.bool "false" "true" b
  show (ASymbol s)    = s
  show (AVector v)    = "!(" ++ showVector 0 ++ ")"
    where showVector i
            | i >= Vec.length v     = ""
            | i == Vec.length v - 1 = show (v Vec.! i)
            | otherwise             = show (v Vec.! i) ++ " " ++ showVector (i + 1)
  show (AProcedure c) = show c
  show (AEnv e)       = show e

instance Type Atom where
  showType a = case a of
    ANil         -> "nil"
    AInt       _ -> "int"
    AFloat     _ -> "float"
    AChar      _ -> "char"
    ABool      _ -> "bool"
    ASymbol    _ -> "symbol"
    AVector    _ -> "vector"
    AProcedure _ -> "procedure"
    AEnv       _ -> "env"

strToAtom :: String -> Atom
strToAtom atom
  | atom == "true"   = ABool True
  | atom == "false"  = ABool False
  | atom == "nil"    = ANil
  | otherwise        = fromMaybe (ASymbol atom) (int <|> float)
  where int   = AInt   <$> (readMaybe atom :: Maybe Int)
        float = AFloat <$> (readMaybe atom :: Maybe Float)
---- atom ----

---- macro ----
data Macro = Macro Point Env Prototype [SExpr]

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
                      EnvMacro (Macro p e prototype sexprs)
                        -> EnvMacro $ Macro p (linsert key value e) prototype sexprs
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
                      EnvMacro (Macro p e prototype sexprs)
                        -> EnvMacro $ Macro p (xinsert key value e) prototype sexprs
                      EnvSExpr (SAtom p (AProcedure (UserDefined e prototype sexprs bound)))
                        -> EnvSExpr . SAtom p . AProcedure $ UserDefined (xinsert key value e) prototype sexprs bound
                      EnvSExpr other
                        -> EnvSExpr other)
             x
xinsert _   _     _                   = undefined

lappend :: Env -> Map String EnvItem -> Env
lappend (Env g args (x:xs)) add = Env g args (x' : xs)
  where x' = fmap (\case
                      EnvMacro (Macro p e prototype sexprs)
                        -> EnvMacro $ Macro p (lappend e add) prototype sexprs
                      EnvSExpr (SAtom p (AProcedure (UserDefined e prototype sexprs bound)))
                        -> EnvSExpr . SAtom p . AProcedure $ UserDefined (lappend e add) prototype sexprs bound
                      EnvSExpr other
                        -> EnvSExpr other)
             (add `Map.union` x)
lappend _                   _   = undefined

xappend :: Env -> Map String EnvItem -> Env
xappend (Env g args (x:xs)) add = Env g args (x' : add : xs)
  where x' = fmap (\case
                      EnvMacro (Macro p e prototype sexprs)
                        -> EnvMacro $ Macro p (xappend e add) prototype sexprs
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
type Eval = ExceptT Fail (ReaderT [Call] IO)

runEval :: Eval a -> IO (Either Fail a)
runEval = (`runReaderT` []) . runExceptT

handleEval :: Eval a -> IO ()
handleEval ev = do
  result <- runEval ev
  case result of
    Right _ -> return ()
    Left f  -> do
      hPrint stderr f

add :: Call -> Eval a -> Eval a
add call = local (call:)
---- eval ----

---- call ----
data Call = Call { cPoint  :: Point
                 , cExpr   :: SExpr }

instance Show Call where
  show (Call point expr) = show point ++ ": " ++ show expr

showStack :: [Call] -> String
showStack = join . reverse . fmap ((++ "\n") . show)

printStack :: [Call] -> IO ()
printStack = putStr . showStack

---- fail ----
data Fail = Fail { lePoint :: Point
                 , leMsg   :: String
                 , leStack :: [Call] }

instance Show Fail where
  show (Fail Undefined msg stack) = showStack stack ++ msg
  show (Fail point     msg stack) = showStack stack ++ show point ++ ": " ++ msg

report :: Point -> String -> Eval a
report point msg = do
  callstack <- ask
  throwError $ Fail point msg callstack

reportUndef :: String -> Eval a
reportUndef = report Undefined

rethrow :: MonadError e m => (e -> e) -> m a -> m a
rethrow f m = catchError m (\e -> throwError $ f e)
---- fail ----
