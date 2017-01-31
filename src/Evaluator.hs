module Evaluator where

-- map
import qualified Data.Map as Map
import Data.Map (Map)

-- other
import Data.Maybe (fromJust)
import Data.IORef
import Control.Monad (foldM)
import Control.Monad.Reader

-- local modules
import Prototype
import Point
import Base
import Util

-- | Expand macros and evaluate a list of s-expressions
expandEvalSeq :: IORef Scope -> [SExpr] -> EvalM [SExpr]
expandEvalSeq scope = processMacros scope >=> evalSeq scope

-- | Evaluate a list of s-expressions
evalSeq :: IORef Scope -> [SExpr] -> EvalM [SExpr]
evalSeq scopeRef = mapM (eval scopeRef)

-- | Expand macros and evaluate a list of s-expressions
-- in a new child scope
expandEvalAloneSeq :: IORef Scope -> [SExpr] -> EvalM [SExpr]
expandEvalAloneSeq scope = processMacros scope >=> evalAloneSeq scope

-- | Evaluate a list of s-expressions, each in a new child scope
evalAloneSeq :: IORef Scope -> [SExpr] -> EvalM [SExpr]
evalAloneSeq scope = mapM (evalAlone scope)

-- | Expand macros and evaluate a list of s-expressions
-- all in a new common child scope 
expandEvalSeqAlone :: IORef Scope -> [SExpr] -> EvalM [SExpr]
expandEvalSeqAlone scope = processMacros scope >=> evalSeqAlone scope

-- | Evaluate a list of s-expressions all in a new common child scope
evalSeqAlone :: IORef Scope -> [SExpr] -> EvalM [SExpr]
evalSeqAlone scopeRef exps = do
  childScope <- liftIO $ newLocal scopeRef
  evalSeq childScope exps

-- | Expand macros and evaluate a list of s-expressions
-- in a new child scope and return the result of the
-- last expression
expandEvalBody :: IORef Scope -> [SExpr] -> EvalM SExpr
expandEvalBody scope = collectMacros scope >=> expandMacros scope >=> evalBody scope

-- | Evaluate a list of s-expressions
-- in a new child scope and return
-- the result of the last expression
evalBody :: IORef Scope -> [SExpr] -> EvalM SExpr
evalBody scopeRef exps = do
  result <- evalSeqAlone scopeRef exps
  return $ if null result then nil else last result

-- | Evaluate an s-expression in a new child scope
evalAlone :: IORef Scope -> SExpr -> EvalM SExpr
evalAlone scope exp = do
  childScope <- liftIO $ newLocal scope
  eval childScope exp

-- | Evaluate an s-expression
eval :: IORef Scope -> SExpr -> EvalM SExpr
eval scopeRef l@(SList p (sFirst:args)) = do
  first <- evalAlone scopeRef sFirst
  rethrow (\fail -> if getPoint fail == Undefined
                    then fail { getPoint = point sFirst }
                    else fail) $
    if isProcedure first
    then eval' (fromProcedure first)
    else reportE (point sFirst) $ "unable to execute s-expression: '" ++ show first ++ "'"
  where eval' :: Procedure -> EvalM SExpr
        eval' pr
          | isUserDefined pr = do
              args' <- evalAloneSeq scopeRef args
              add (Call p $ SList p (procedure pr:args')) $ call scopeRef (point sFirst) pr args'
          | isMacro pr                       = do
              exp <- call scopeRef (point sFirst) pr args
              add (Call p $ SList p (procedure pr:args)) $ eval scopeRef exp
          | otherwise                        = -- isBuiltIn pr
              add (Call p $ SList p (procedure pr:args)) $ call scopeRef (point sFirst) pr args
eval _ (SAtom p (ASymbol "_"))          = reportE p "addressing '_' is forbidden"
eval scopeRef (SAtom p (ASymbol sym))   = do
  result <- liftIO $ exploreIORefIO scopeRef (scLookup sym)
  case result of
    Just s -> return $ setPoint p s
    _      -> reportE p $ "undefined identificator '" ++ sym ++ "'"
eval _ other                            = return other

-- | Create bindings from a function prototype and
-- | and the actual arguments
bindArgs :: Prototype -> [SExpr] -> EvalM (Map String SExpr)
bindArgs (Prototype argNames optNames Nothing) args
  | length args > l1 + l2 = reportE' "too many arguments"
  | length args < l1      = reportE' "too little arguments"
  | otherwise             = return $ Map.fromList $ zip (argNames ++ optNames) (args ++ repeat nil)
    where l1 = length argNames
          l2 = length optNames
bindArgs (Prototype argNames optNames rest) args
  | l < l1      = reportE' "too little arguments"
  | l < l1 + l2 = return . Map.fromList $ zip names (args ++ take (l1 + l2 - l) (repeat nil) ++ [list []])
  | otherwise   = let (left, right) = splitAt (l1 + l2) args
                      args'         = left ++ [list right]
                  in return $ Map.fromList $ zip names args'
  where restName = case fromJust rest of
          Rest x -> x
          Body x -> x
        names = argNames ++ optNames ++ [restName]
        l1 = length argNames
        l2 = length optNames
        l  = length args

data PLLStatus = PLLNone | PLLOptional | PLLRest | PLLBody | PLLEnd

-- | Take an s-list of the form (args... [&optional optional-args...] [(&rest|&body) lastArg])
-- | and constructs a Prototype
parseLambdaList :: SExpr -> EvalM Prototype
parseLambdaList exp = parseLambdaList' PLLNone (Prototype [] [] Nothing) =<< getList exp
  where parseLambdaList' :: PLLStatus -> Prototype -> [SExpr] -> EvalM Prototype
        parseLambdaList' PLLNone prototype (x:xs) = do
          x' <- getSymbol x
          case x' of
            "&optional" -> parseLambdaList' PLLOptional prototype xs
            "&rest"     -> parseLambdaList' PLLRest     prototype xs
            "&body"     -> parseLambdaList' PLLBody     prototype xs
            other       -> parseLambdaList' PLLNone     prototype { Prototype.getArgs =
                                                                      Prototype.getArgs prototype ++ [other] } xs
        parseLambdaList' PLLNone prototype []     = return prototype

        parseLambdaList' PLLOptional prototype (x:xs) = do
          x' <- getSymbol x
          case x' of
            "&optional" -> reportE (point x) "more than one &optional in a lambda list"
            "&rest"     -> parseLambdaList' PLLRest     prototype xs
            "&body"     -> parseLambdaList' PLLBody     prototype xs
            other       -> parseLambdaList' PLLOptional prototype { getOptional =
                                                                      getOptional prototype ++ [other] } xs
        parseLambdaList' PLLOptional prototype []     = return prototype

        parseLambdaList' PLLRest prototype (x:xs) = do
          x' <- getSymbol x
          case x' of
            "&optional" -> reportE (point x) "no &optional after &rest expected"
            "&rest"     -> reportE (point x) "no &rest after &rest expected"
            "&body"     -> reportE (point x) "no &body after &rest expected"
            other       -> parseLambdaList' PLLEnd prototype { getRest = Just (Rest other) } xs
        parseLambdaList' PLLRest _         []     = reportE' "a symbol after &rest expected"

        parseLambdaList' PLLBody prototype (x:xs) = do
          x' <- getSymbol x
          case x' of
            "&optional" -> reportE (point x) "no &optional after &body expected"
            "&rest"     -> reportE (point x) "no &rest after &body expected"
            "&body"     -> reportE (point x) "no &body after &body expected"
            other       -> parseLambdaList' PLLEnd prototype { getRest = Just (Body other) } xs
        parseLambdaList' PLLBody _         []     = reportE' "a symbol after &body expected"

        parseLambdaList' PLLEnd prototype []      = return prototype
        parseLambdaList' PLLEnd _         (x:xs)  = reportE (point x) "nothing expected at the end of the lambda list"

-- | Collect macro definitions and expand them
processMacros :: IORef Scope -> [SExpr] -> EvalM [SExpr]
processMacros scopeRef = collectMacros scopeRef >=> expandMacros scopeRef

-- | Take a scope and evaluate all top-level
-- defmacros in it, return the remaining s-expressions
collectMacros :: IORef Scope -> [SExpr] -> EvalM [SExpr]
collectMacros scopeRef = foldM (\acc sexpr -> do
                                   defmacro <- parseDefmacro scopeRef sexpr
                                   case defmacro of
                                     Just (name, macro) -> do liftIO $ modifyIORef scopeRef $ scInsert name $ procedure macro
                                                              return acc
                                     Nothing            -> return $ acc ++ [sexpr])
                         []

-- | Expand all macros in a scope
expandMacros :: IORef Scope -> [SExpr] -> EvalM [SExpr]
expandMacros scope = mapM (expandMacro scope)

data EMState = Default | Backquote

-- | Expand one macro expression recursively
expandMacro :: IORef Scope -> SExpr -> EvalM SExpr
expandMacro scopeRef = expandMacro' Default
  where expandMacro' :: EMState -> SExpr -> EvalM SExpr
        expandMacro' Default l@(SList p (first@(SAtom _ (ASymbol sym)):rest))
          | sym == "quote"       = return l
          | sym == "backquote"   = do
              rest' <- mapM (expandMacro' Backquote) rest
              return $ SList p (first:rest')
          | sym == "interpolate" = reportE p "calling 'interpolate' out of backquote"
          | otherwise = do
              result <- liftIO $ exploreIORefIO scopeRef (scLookup sym)
              case result of
                Just (SAtom p (AProcedure m@(Macro _ _ _ _)))  -> do
                  expr <- call scopeRef p m rest
                  expandMacro' Default expr
                _                       -> do
                  list' <- mapM (expandMacro' Default) (first:rest)
                  return $ SList p list'
        expandMacro' Default l@(SList p (first:rest)) = do
          first' <- expandMacro' Default first
          if isSymbol first'
            then expandMacro' Default $ SList p (first':rest)
            else do
              rest' <- mapM (expandMacro' Default) rest
              return $ SList p (first':rest')
        expandMacro' Default other                    = return other
        expandMacro' Backquote l@(SList p (first@(SAtom _ (ASymbol sym)):rest))
          | sym == "interpolate" = do
              rest' <- mapM (expandMacro' Default) rest
              return $ SList p (first:rest')
          | otherwise            = return l
        expandMacro' Backquote other = return other

-- | Parse a defmacro expression
parseDefmacro :: IORef Scope -> SExpr -> EvalM (Maybe (String, Procedure))
parseDefmacro scopeRef (SList p (SAtom _ (ASymbol "defmacro"):sName:lambdaList:body)) = do
  name <- getSymbol sName
  prototype <- parseLambdaList lambdaList
  return $ Just (name, Macro scopeRef prototype body [])
parseDefmacro _ (SList p (SAtom _ (ASymbol "defmacro"):_)) = reportE p "at least two arguments required"
parseDefmacro _ _                                          = return Nothing

-- | Bind arguments to a procedure or special operator
bind :: Procedure -> [SExpr] -> EvalM Procedure
bind (UserDefined scope prototype@(Prototype _ _ Nothing) sexprs bound) args =
  return $ UserDefined scope prototype sexprs (bound ++ args)
bind (UserDefined scope prototype@(Prototype argNames optNames _) sexprs bound) args
  | length args + length args > length argNames + length optNames = reportE' "too many arguments"
  | otherwise                                                     =
      return $ UserDefined scope prototype sexprs (bound ++ args)
bind (Macro scope prototype@(Prototype _ _ Nothing) sexprs bound) args =
  return $ UserDefined scope prototype sexprs (bound ++ args)
bind (Macro scope prototype@(Prototype argNames optNames _) sexprs bound) args
  | length args + length args > length argNames + length optNames = reportE' "too many arguments"
  | otherwise                                                     =
      return $ UserDefined scope prototype sexprs (bound ++ args)
bind (BuiltIn name (Just argsCount) f bound) args
  | argsCount < (length bound + length args) = reportE' "too many arguments"
  | otherwise                                = return $ BuiltIn name (Just argsCount) f (bound ++ args)
bind (BuiltIn name Nothing f bound) args = return $ BuiltIn name Nothing f (bound ++ args)

-- | Call a procedure or special operator
call :: IORef Scope -> Point -> Procedure -> [SExpr] -> EvalM SExpr
call scopeRef p pr args = fmap (setPoint p) $ case pr of
  UserDefined localScope prototype exps bound -> do
    bindings <- bindArgs prototype (bound ++ args)
    childScope <- liftIO $ newLocal' bindings localScope
    evalBody childScope exps
  Macro localScope prototype exps bound       -> do
    bindings <- bindArgs prototype args
    childScope <- liftIO $ newLocal' bindings localScope
    setPoint p <$> expandEvalBody childScope exps
  BuiltIn _ _ f bound                         -> f scopeRef (bound ++ args)

-- | Evaluate arguments and pass them to function f
withEvaluatedArgs :: (IORef Scope -> [SExpr] -> EvalM SExpr) -> IORef Scope -> [SExpr] -> EvalM SExpr
withEvaluatedArgs f scopeRef = mapM (evalAlone scopeRef) >=> f scopeRef
