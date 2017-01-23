module Base where

-- maps
import qualified Data.Map as Map
import Data.Map (Map)

-- vectors
import qualified Data.Vector as Vec
import Data.Vector (Vector)

-- other
import qualified Control.Conditional as C
import Data.IORef
import Control.Monad.Except
import Control.Monad.Reader
import Control.Applicative ((<|>))
import Control.Arrow
import Control.Exception (Exception)
import System.IO (hPrint, stderr)
import Data.Maybe
import Text.Read (readMaybe)

-- local modules
import Prototype
import Point
import Type
import Util

---- s-expression ----
data SExpr = SList Point [SExpr] | SAtom Point Atom

isList :: SExpr -> Bool
isList (SList _ _) = True
isList _           = False

isEmptyList :: SExpr -> Bool
isEmptyList = (isList &&& null . fromList) >>> uncurry (&&)

isAtom :: SExpr -> Bool
isAtom (SAtom _ _) = True
isAtom _           = False

fromList :: SExpr -> [SExpr]
fromList (SList _ xs) = xs
fromList _            = undefined

fromAtom :: SExpr -> Atom
fromAtom (SAtom _ a) = a
fromAtom _           = undefined

getList :: SExpr -> Lisp [SExpr]
getList = unpackSExpr fromList "list"

getAtom :: SExpr -> Lisp Atom
getAtom = unpackSExpr fromAtom "atom"

nil :: SExpr
nil = SAtom Undefined ANil

atom :: Atom -> SExpr
atom = SAtom Undefined

list :: [SExpr] -> SExpr
list = SList Undefined

point :: SExpr -> Point
point (SList p _) = p
point (SAtom p _) = p

setPoint :: Point -> SExpr -> SExpr
setPoint p (SList _ xs) = SList p xs
setPoint p (SAtom _ a)  = SAtom p a

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
    | otherwise        = show $ fromAtom expr -- isAtom
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
    | otherwise    = showType $ fromAtom sexpr -- isAtom

isNil, isInt, isFloat, isChar, isBool, isSymbol, isVector, isProcedure, isEnv, isSequence, isNumber, isString :: SExpr -> Bool

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

isSequence = isList &&& isVector >>> uncurry (||)

isNumber = isFloat &&& isInt >>> uncurry (||)

isString = isList &&& (all isChar . fromList) >>> uncurry (&&)

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

fromEnv :: SExpr -> Map String Binding
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

unpackSExpr :: (SExpr -> a) -> String -> SExpr -> Lisp a
unpackSExpr fromA msg exp = fromFalsumM (reportE (point exp) (msg ++ " expected")) (fromA exp)

getInt :: SExpr -> Lisp Int
getInt = unpackSExpr fromInt "int"

getFloat :: SExpr -> Lisp Float
getFloat = unpackSExpr fromFloat "float"

getChar :: SExpr -> Lisp Char
getChar = unpackSExpr fromChar "char"

getBool :: SExpr -> Lisp Bool
getBool = unpackSExpr fromBool "bool"

getSymbol :: SExpr -> Lisp String
getSymbol = unpackSExpr fromSymbol "symbol"

getVector :: SExpr -> Lisp (Vector SExpr)
getVector = unpackSExpr fromVector "vector"

getProcedure :: SExpr -> Lisp Procedure
getProcedure = unpackSExpr fromProcedure "procedure"

getEnv :: SExpr -> Lisp (Map String Binding)
getEnv = unpackSExpr fromEnv "env"

getSequence :: SExpr -> Lisp [SExpr]
getSequence = unpackSExpr fromSequence "sequence"

getNumber :: SExpr -> Lisp Float
getNumber = unpackSExpr fromNumber "number"

getString :: SExpr -> Lisp String
getString = unpackSExpr fromString "string"

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

env :: Map String Binding -> SExpr
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
          | AEnv       (Map String Binding)

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
data Macro = Macro Point (IORef Scope) Prototype [SExpr]

instance Eq Macro where
  (==) = undefined

instance Show Macro where
  show = const "#<macro>"
---- macro ----

---- procedure ----
data Procedure = UserDefined (IORef Scope) Prototype [SExpr] [SExpr]
               | BuiltIn     String (Maybe Int) (IORef Scope -> [SExpr] -> Lisp SExpr) [SExpr]
               | SpecialOp   String (Maybe Int) (IORef Scope -> [SExpr] -> Lisp SExpr) [SExpr]

isUserDefined, isBuiltIn, isSpecialOp :: Procedure -> Bool

isUserDefined UserDefined {} = True
isUserDefined _              = False

isBuiltIn BuiltIn {} = True
isBuiltIn _          = False

isSpecialOp SpecialOp {} = True
isSpecialOp _            = False

instance Show Procedure where
  show UserDefined {}         = "#<procedure>"
  show (BuiltIn name _ _ _)   = "#<procedure:" ++ name ++ ">"
  show (SpecialOp name _ _ _) = "#<special operator:" ++ name ++ ">"
---- procedure ----

---- scope ----
data Scope = Scope { getBindings :: Map String Binding
                   , getG        :: Int
                   , getCmdArgs  :: [String]
                   , getParent   :: Maybe (IORef Scope) }

data Binding = BSExpr SExpr | BMacro Macro
  deriving Eq

instance Show Binding where
  show (BSExpr exp)   = show exp
  show (BMacro macro) = show macro

setBindings :: Map String Binding -> Scope -> Scope
setBindings bindings scope = scope { getBindings = bindings }

setG :: Int -> Scope -> Scope
setG g scope = scope { getG = g }

setCmdArgs :: [String] -> Scope -> Scope
setCmdArgs args scope = scope { getCmdArgs = args }

setParent :: Maybe (IORef Scope) -> Scope -> Scope
setParent parent scope = scope { getParent = parent }

newGlobal :: [String] -> Scope
newGlobal = newGlobal' Map.empty

newGlobal' :: Map String Binding -> [String] -> Scope
newGlobal' bindings args = Scope { getBindings = bindings
                                 , getG        = 0
                                 , getCmdArgs  = args
                                 , getParent   = Nothing }

newLocal :: IORef Scope -> IO (IORef Scope)
newLocal = newLocal' Map.empty

newLocal' :: Map String Binding -> IORef Scope -> IO (IORef Scope)
newLocal' bindings parentRef = do
  g <- exploreIORef parentRef getG
  cmdArgs <- exploreIORef parentRef getCmdArgs
  newIORef Scope { getBindings = bindings
                 , getG        = g
                 , getCmdArgs  = cmdArgs
                 , getParent   = Just parentRef }

isGlobal :: Scope -> Bool
isGlobal = isNothing . getParent

isChild :: Scope -> Bool
isChild = isJust . getParent

scLookup :: String -> Scope -> IO (Maybe Binding)
scLookup key scope = case Map.lookup key (getBindings scope) of
                       Just value -> return $ Just value
                       Nothing    -> case getParent scope of
                         Just parent -> exploreIORefIO parent (scLookup key)
                         Nothing     -> return Nothing

scLookupM :: String -> Scope -> IO (Maybe Macro)
scLookupM key scope = case Map.lookup key (getBindings scope) of
                            Just (BMacro m) -> return $ Just m
                            _               -> case getParent scope of
                              Just parent -> exploreIORefIO parent (scLookupM key)
                              Nothing     -> return Nothing

scLookupS :: String -> Scope -> IO (Maybe SExpr)
scLookupS key scope = case Map.lookup key (getBindings scope) of
                            Just (BSExpr exp) -> return $ Just exp
                            _                 -> case getParent scope of
                              Just parent -> exploreIORefIO parent (scLookupS key)
                              Nothing     -> return Nothing

scMember :: String -> Scope -> IO Bool
scMember key = fmap isJust . scLookup key

scMemberM :: String -> Scope -> IO Bool
scMemberM key = fmap isJust . scLookupM key

scMemberS :: String -> Scope -> IO Bool
scMemberS key = fmap isJust . scLookupS key

scDelete :: String -> Scope -> IO Scope
scDelete key scope
  | isGlobal scope = return scope'
  | otherwise      = do
      modifyIORefIO parentRef (scDelete key)
      return scope'
  where scope'    = scope { getBindings = Map.delete key (getBindings scope) }
        parentRef = fromJust $ getParent scope

scInsert :: String -> Binding -> Scope -> Scope
scInsert key value scope = scope { getBindings = Map.insert key value (getBindings scope) }

scAppend :: Map String Binding -> Scope -> Scope
scAppend add scope = scope { getBindings = Map.union add (getBindings scope) }

scSet :: String -> Binding -> Scope -> IO Scope
scSet key value scope = case Map.lookup key (getBindings scope) of
  Just  _ -> return $ scInsert key value scope
  Nothing -> case getParent scope of
    Just parent -> do modifyIORefIO parent (scSet key value)
                      return scope
    Nothing     -> return scope
instance Show Scope where
  show _ = "#<TODO: show Scope>"
---- scope ----

---- lisp monad ---
type Lisp = ExceptT Fail (ReaderT [Call] IO)

runLisp :: Lisp a -> IO (Either Fail a)
runLisp = flip runReaderT [] . runExceptT

forwardExcept :: MonadError Fail m => Except Fail a -> m a
forwardExcept m = case runExcept m of
  Left fail -> throwError fail
  Right val -> return val

handleLisp ::  Lisp a -> IO ()
handleLisp = runLisp >=> \case
  Right _ -> return ()
  Left  f -> hPrint stderr f

add :: Call -> Lisp a -> Lisp a
add call = local (call:)
---- lisp monad ----

---- call ----
data Call = Call { cPoint :: Point
                 , cExpr  :: SExpr }

instance Show Call where
  show (Call point expr) = show point ++ ": " ++ show expr

showStack :: [Call] -> String
showStack = join . reverse . fmap ((++ "\n") . show)

printStack :: [Call] -> IO ()
printStack = putStr . showStack

---- fail ----
data Fail = ReadFail { getPoint :: Point
                     , getMsg   :: String }
          | EvalFail { getPoint :: Point
                     , getMsg :: String
                     , getStack :: [Call] }

instance Exception Fail

instance Show Fail where
  show (ReadFail Undefined msg) = msg
  show (ReadFail point     msg) = show point ++ ": " ++ msg
  show (EvalFail Undefined msg stack) = showStack stack ++ msg
  show (EvalFail point     msg stack) = showStack stack ++ show point ++ ": " ++ msg

reportE :: Point -> String -> Lisp a
reportE point msg = do
  callstack <- ask
  throwError $ EvalFail point msg callstack

reportE' :: String -> Lisp a
reportE' = reportE Undefined

reportR :: MonadError Fail m => Point -> String -> m a
reportR point = throwError . ReadFail point

reportR' :: MonadError Fail m => String -> m a
reportR' = reportR Undefined

rethrow :: MonadError e m => (e -> e) -> m a -> m a
rethrow f m = catchError m (throwError . f)
---- fail ----
