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

getList :: SExpr -> EvalM [SExpr]
getList = unpackSExpr fromList "list"

getAtom :: SExpr -> EvalM Atom
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

isNil, isInt, isFloat, isChar, isBool, isSymbol, isVector, isProcedure, isSequence, isNumber, isString :: SExpr -> Bool

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

unpackSExpr :: (SExpr -> a) -> String -> SExpr -> EvalM a
unpackSExpr fromA msg exp = fromFalsumM (reportE (point exp) (msg ++ " expected")) (fromA exp)

getInt :: SExpr -> EvalM Int
getInt = unpackSExpr fromInt "int"

getFloat :: SExpr -> EvalM Float
getFloat = unpackSExpr fromFloat "float"

getChar :: SExpr -> EvalM Char
getChar = unpackSExpr fromChar "char"

getBool :: SExpr -> EvalM Bool
getBool = unpackSExpr fromBool "bool"

getSymbol :: SExpr -> EvalM String
getSymbol = unpackSExpr fromSymbol "symbol"

getVector :: SExpr -> EvalM (Vector SExpr)
getVector = unpackSExpr fromVector "vector"

getProcedure :: SExpr -> EvalM Procedure
getProcedure = unpackSExpr fromProcedure "procedure"

getSequence :: SExpr -> EvalM [SExpr]
getSequence = unpackSExpr fromSequence "sequence"

getNumber :: SExpr -> EvalM Float
getNumber = unpackSExpr fromNumber "number"

getString :: SExpr -> EvalM String
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

strToAtom :: String -> Atom
strToAtom atom
  | atom == "true"   = ABool True
  | atom == "false"  = ABool False
  | atom == "nil"    = ANil
  | otherwise        = fromMaybe (ASymbol atom) (int <|> float)
  where int   = AInt   <$> (readMaybe atom :: Maybe Int)
        float = AFloat <$> (readMaybe atom :: Maybe Float)
---- atom ----

---- procedure ----
data Procedure = UserDefined (IORef Scope) Prototype [SExpr] [SExpr]
               | BuiltIn   String (Maybe Int) (IORef Scope -> [SExpr] -> EvalM SExpr) [SExpr]
               | Macro       (IORef Scope) Prototype [SExpr] [SExpr]

isUserDefined, isBuiltIn, isMacro :: Procedure -> Bool

isUserDefined UserDefined {} = True
isUserDefined _              = False

isBuiltIn BuiltIn {} = True
isBuiltIn _            = False

isMacro Macro {} = True
isMacro _        = False

instance Show Procedure where
  show UserDefined {}         = "#<procedure>"
  show (BuiltIn name _ _ _) = "#<procedure:" ++ name ++ ">"
  show (Macro name _ _ _)     = "#<macro>"
---- procedure ----

---- scope ----
data Scope = Scope { getBindings :: Map String SExpr
                   , getG        :: Int
                   , getCmdArgs  :: [String]
                   , getImports  :: [IORef Scope] }

modifyBindings :: (Map String SExpr -> Map String SExpr) -> Scope -> Scope
modifyBindings f scope = scope { getBindings = f $ getBindings scope }

modifyG :: (Int -> Int) -> Scope -> Scope
modifyG f scope = scope { getG = f $ getG scope }

modifyCmdArgs :: ([String] -> [String]) -> Scope -> Scope
modifyCmdArgs f scope = scope { getCmdArgs = f $ getCmdArgs scope }

modifyImports :: ([IORef Scope] -> [IORef Scope]) -> Scope -> Scope
modifyImports f scope = scope { getImports = f $ getImports scope }

newGlobal :: [String] -> Scope
newGlobal = newGlobal' Map.empty

newGlobal' :: Map String SExpr -> [String] -> Scope
newGlobal' bindings args = Scope { getBindings = bindings
                                 , getG        = 0
                                 , getCmdArgs  = args
                                 , getImports  = [] }

newLocal :: IORef Scope -> IO (IORef Scope)
newLocal = newLocal' Map.empty

newLocal' :: Map String SExpr -> IORef Scope -> IO (IORef Scope)
newLocal' bindings parentRef = do
  g <- exploreIORef parentRef getG
  cmdArgs <- exploreIORef parentRef getCmdArgs
  newIORef Scope { getBindings = bindings
                 , getG        = g
                 , getCmdArgs  = cmdArgs
                 , getImports  = [parentRef] }

isGlobal :: Scope -> Bool
isGlobal = null . getImports

isChild :: Scope -> Bool
isChild = not . isGlobal

-- | Return the first encountered (Just _), never touch the rest.
maybeOrM :: [IO (Maybe a)] -> IO (Maybe a)
maybeOrM (x:xs) = do
  x' <- x
  if isJust x'
    then return x'
    else maybeOrM xs
maybeOrM []     = return Nothing

scLookup :: String -> Scope -> IO (Maybe SExpr)
scLookup key scope = case Map.lookup key (getBindings scope) of
                       Just value -> return $ Just value
                       Nothing    -> let imports = map readIORef $ getImports scope
                                         lookups = map (scLookup key =<<) imports
                                     in maybeOrM lookups

scMember :: String -> Scope -> IO Bool
scMember key = fmap isJust . scLookup key

scDelete :: String -> Scope -> IO Scope
scDelete key scope = do
  mapM_ (scDelete key <=< readIORef) $ getImports scope
  return scope { getBindings = Map.delete key (getBindings scope) }

scInsert :: String -> SExpr -> Scope -> Scope
scInsert key value scope = scope { getBindings = Map.insert key value (getBindings scope) }

scAppend :: Map String SExpr -> Scope -> Scope
scAppend add scope = scope { getBindings = Map.union add (getBindings scope) }

-- TODO: fix it, because it barely works
scSet :: String -> SExpr -> Scope -> IO Scope
scSet key value scope = case Map.lookup key (getBindings scope) of
  Just  _ -> return $ scInsert key value scope
  Nothing -> do
    mapM (flip modifyIORefIO $ scSet key value) $ getImports scope
    return scope

instance Show Scope where
  show (Scope bindings g cmdArgs imports) = "#<scope: bindings: " ++ showBindings bindings ++
                                            "\n                g: " ++ show g ++
                                            "\n          cmdArgs: " ++ show cmdArgs ++
                                            "\n          imports: " ++ show (length imports) ++ ">"
    where showBindings = Map.foldMapWithKey (\key value -> "\n(" ++ show key ++ " " ++ show value ++ ")")

{-showScope :: Scope -> IO String
showScope scope = case getParent scope of
  Just parent -> do begin <- showScope =<< readIORef parent
                    return $ begin ++ "\n" ++ show scope
  Nothing     -> return $ show scope

printScope :: Scope -> IO ()
printScope = putStrLn <=< showScope-}
---- scope ----

---- lisp monad ---
type EvalM = ExceptT Fail (ReaderT [Call] IO)

runEvalM :: EvalM a -> IO (Either Fail a)
runEvalM = flip runReaderT [] . runExceptT

forwardExcept :: MonadError Fail m => Except Fail a -> m a
forwardExcept m = case runExcept m of
  Left fail -> throwError fail
  Right val -> return val

handleEvalM ::  EvalM a -> IO ()
handleEvalM = runEvalM >=> \case
  Right _ -> return ()
  Left  f -> hPrint stderr f

add :: Call -> EvalM a -> EvalM a
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

reportE :: Point -> String -> EvalM a
reportE point msg = do
  callstack <- ask
  throwError $ EvalFail point msg callstack

reportE' :: String -> EvalM a
reportE' = reportE Undefined

reportR :: MonadError Fail m => Point -> String -> m a
reportR point = throwError . ReadFail point

reportR' :: MonadError Fail m => String -> m a
reportR' = reportR Undefined

rethrow :: MonadError e m => (e -> e) -> m a -> m a
rethrow f m = catchError m (throwError . f)
---- fail ----
