module Interpreter (interpreteProgram
                   ,interpreteModule
                   ,repl) where

-- map
import qualified Data.Map as Map
import Data.Map (Map)

-- other
import Data.IORef
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import System.Console.Readline
import System.IO

-- local modules
import qualified Reader as R
import qualified Evaluator as E
import Lib.Everything
import Base
import Point
import Util

preludePath :: String
preludePath = "stdlib/prelude.unlisp"

-- | Interprete a module and returns its scope
interpreteModule :: Bool -> String -> Lisp (IORef Scope)
interpreteModule prelude filename = do
  scope <- liftIO $ loadEnv prelude
  text <- liftIO $ readFile filename
  childScope <- liftIO $ newLocal scope
  exps <- forwardExcept $ R.read (startPoint filename) text
  exps' <- preprocess childScope exps
  E.expandEvalSeq childScope exps'
  return childScope

-- | A lisp interpretator is just a reader and evaluator joined together
interpreteProgram :: Bool -> String -> [String] -> IO ()
interpreteProgram prelude filename args = do
  scopeRef <- loadEnv prelude
  modifyIORef scopeRef (modifyCmdArgs $ const args)
  text <- readFile filename
  handleLisp $ do
    exps <- forwardExcept $ R.read (startPoint filename) text
    exps' <- preprocess scopeRef exps
    E.expandEvalBody scopeRef exps'

-- | REPL (read-eval-print-loop) environment
repl :: Bool -> IO ()
repl prelude = do
  scope <- loadEnv prelude
  childScope <- newLocal scope
  handleLines (startPoint "<REPL>") childScope
    where handleLines :: Point -> IORef Scope -> IO ()
          handleLines p scopeRef = do
            line <- readline $ "[" ++ show (pRow p) ++ "]> "

            case line of
              Just line -> do
                unless (null line) $ addHistory line
                result <- runLisp $ do
                  exps <- forwardExcept $ R.read p line
                  exps' <- preprocess scopeRef exps
                  result <- E.expandEvalSeq scopeRef exps'
                  return $ if null result then nil else last result

                exp <- case result of
                  Right val -> return val
                  Left  f   -> do
                    hPrint stderr f
                    return nil

                unless (isNil exp) (putStrLn $ "=> " ++ show exp)
                modifyIORef scopeRef (scInsert "it" $ BSExpr exp)
                handleLines (forwardRow p) scopeRef
              Nothing   -> putStrLn "Bye!"

-- | returns start environment plus prelude
biInitialEnv :: IORef Scope -> [SExpr] -> Lisp SExpr
biInitialEnv _ [] = liftIO $ do
  scope <- loadPrelude
  env <$> exploreIORef scope getBindings
biInitialEnv _ _  = reportE' "no arguments requried"

-- | Load start environment.
-- No prelude if the first argument is false
loadEnv :: Bool -> IO (IORef Scope)
loadEnv True  = loadPrelude
loadEnv False = newIORef $ newGlobal' startEnv []

-- | loads prelude and start environment
loadPrelude :: IO (IORef Scope)
loadPrelude = do
  text <- readFile preludePath
  global <- newIORef $ newGlobal' startEnv []
  let result = runExcept $ R.read (startPoint preludePath) text
  case result of
    Right exps -> handleLisp $ E.expandEvalSeq global exps
    Left fail  -> hPrint stderr fail
  return global

-- | start environment
-- | contains built-in functions and special operators
startEnv :: Map String Binding
startEnv = Map.fromList $
  fmap (\(name, args, f) -> (name, BSExpr . procedure $ SpecialOp name args f [])) specialOperators ++
  fmap (\(name, args, f) -> (name, BSExpr . procedure $ BuiltIn name args f []))  (builtinFunctions ++
     [("initial-env", Just 0, biInitialEnv)])

-- | Do all preprocessing.
-- For now it's just importing modules.
preprocess :: IORef Scope -> [SExpr] -> Lisp [SExpr]
preprocess = collectImports

-- | Collect imports in the scope
collectImports :: IORef Scope -> [SExpr] -> Lisp [SExpr]
collectImports scopeRef = foldM (\acc exp -> do
                                    import' <- parseImport exp
                                    case import' of
                                      Just filename -> do moduleScope <- interpreteModule True filename
                                                          liftIO $ modifyIORef scopeRef $ modifyImports (moduleScope:)
                                                          return acc
                                      Nothing       -> return $ acc ++ [exp])
                          []

-- | Parse an import expression
parseImport :: SExpr -> Lisp (Maybe String)
parseImport (SList p [SAtom _ (ASymbol "import"), filename])
  | not $ isList filename    = reportE p "string expected"
  | null $ fromList filename = reportE p "module name cannot be empty"
  | otherwise                = Just <$> (mapM Base.getChar . tail $ fromList filename)
parseImport (SList p (SAtom _ (ASymbol "import"):_))         = reportE p "just one argument required"
parseImport _                                                = return Nothing
