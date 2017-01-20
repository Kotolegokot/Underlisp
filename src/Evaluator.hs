module Evaluator where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust)
import Control.Monad (foldM, void)
import Control.Monad.Reader
import Control.Monad.State
import Prototype
import Point
import Base

-- | evaluates a module with the given environment and returns nothing
evaluateProgram ::[String] -> [SExpr] -> Lisp ()
evaluateProgram args body = do
  modify $ setArgs args
  void $ expandAndEvalScope body

-- | evaluates a module with the given environment
evaluateModule :: [SExpr] -> Lisp (Map String EnvItem)
evaluateModule body = do
  modify passIn
  void $ expandAndEvalScope body
  result <- get
  modify passOut
  return $ lexical result

-- | expands macros and evaluates a scope
expandAndEvalScope :: [SExpr] -> Lisp SExpr
expandAndEvalScope = collectMacros >=> expandMacros >=> evalScope

evalScope :: [SExpr] -> Lisp SExpr
evalScope exps = do
  modify passIn
  result <- mapM eval exps
  modify passOut
  return $ last result

-- | Expands macros and evaluates a scope as if
-- | it is at the same lexical level
expandAndEvalScopeInterpolated :: [SExpr] -> Lisp SExpr
expandAndEvalScopeInterpolated = collectMacros >=> expandMacros >=> evalScopeInterpolated

-- | Evaluates lexical scope
evalScopeInterpolated :: [SExpr] -> Lisp SExpr
evalScopeInterpolated = return . last <=< mapM eval

-- | Evaluate an s-expression writing into call stack
-- | if any function is invoked
eval :: SExpr -> Lisp SExpr
eval l@(SList p (sFirst:args)) = do
  first <- eval sFirst
  rethrow (\fail -> if getPoint fail == Undefined
                    then fail { getPoint = point sFirst }
                    else fail) $
    if isProcedure first
    then eval' (fromProcedure first)
    else reportE (point sFirst) $ "unable to execute s-expression: '" ++ show first ++ "'"
  where eval' :: Procedure -> Lisp SExpr
        eval' pr
          | isUserDefined pr || isBuiltIn pr = do
              args' <- mapM eval args
              add (Call p $ SList p (procedure pr:args')) $ call (point sFirst) pr args'
          | otherwise                        = -- isSpecialOp
              add (Call p $ SList p (procedure pr:args)) $ call (point sFirst) pr args
eval (SAtom p (ASymbol "_"))   = reportE p "addressing '_' is forbidden"
eval (SAtom p (ASymbol sym))   = do
  result <- gets $ envLookup sym
  case result of
    Just (EnvSExpr s) -> return $ setPoint s p
    _                 -> reportE p $ "undefined identificator '" ++ sym ++ "'"
eval other                     = return other

-- | Create bindings from a function prototype and
-- | and the actual arguments
bindArgs :: Prototype -> [SExpr] -> Lisp (Map String EnvItem)
bindArgs (Prototype argNames optNames Nothing) args
  | length args > l1 + l2 = reportE' "too many arguments"
  | length args < l1      = reportE' "too little arguments"
  | otherwise             = return $ Map.fromList $ zip (argNames ++ optNames) (map EnvSExpr $ args ++ repeat nil)
    where l1 = length argNames
          l2 = length optNames
bindArgs (Prototype argNames optNames rest) args
  | l < l1      = reportE' "too little arguments"
  | l < l1 + l2 = return . Map.fromList $ zip names (map EnvSExpr $ args ++ take (l1 + l2 - l) (repeat nil) ++ [list []])
  | otherwise   = let (left, right) = splitAt (l1 + l2) args
                      args'         = left ++ [list right]
                  in return $ Map.fromList $ zip names (map EnvSExpr args')
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
parseLambdaList :: SExpr -> Lisp Prototype
parseLambdaList exp = parseLambdaList' PLLNone (Prototype [] [] Nothing) =<< getList exp
  where parseLambdaList' :: PLLStatus -> Prototype -> [SExpr] -> Lisp Prototype
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

-- | invokes a macro with the given environment and arguments
callMacro :: Macro -> [SExpr] -> Lisp SExpr
callMacro (Macro p localE prototype sexprs) args = do
  previous <- get
  argBindings <- bindArgs prototype args
  put localE
  modify $ lappend argBindings
  evaluated <- expandAndEvalScope sexprs
  put previous
  return $ setPoint evaluated p

-- | Take a scope and evaluate all top-level
-- | defmacros in it, return the remaining s-expressions
collectMacros :: [SExpr] -> Lisp [SExpr]
collectMacros = foldM (\acc sexpr -> do
                          defmacro <- parseDefmacro sexpr
                          case defmacro of
                            Just (name, macro) -> do modify (linsert name $ EnvMacro macro)
                                                     return acc
                            Nothing            -> return $ acc ++ [sexpr])
                []

-- | expands all top-level macros
expandMacros :: [SExpr] -> Lisp [SExpr]
expandMacros = mapM expandMacro

data EMState = Default | Backquote

-- | expands one macro expression recursively
expandMacro :: SExpr -> Lisp SExpr
expandMacro = expandMacro' Default
  where expandMacro' :: EMState -> SExpr -> Lisp SExpr
        expandMacro' Default l@(SList p (first@(SAtom _ (ASymbol sym)):rest))
          | sym == "quote"       = return l
          | sym == "backquote"   = do
              rest' <- mapM (expandMacro' Backquote) rest
              return $ SList p (first:rest')
          | sym == "interpolate" = reportE p "calling out of backquote"
          | otherwise = do
              result <- gets $ lookupMacro sym
              case result of
                Just m  -> do
                  expr <- callMacro m rest
                  expandMacro' Default expr
                Nothing -> do
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

-- | parses a defmacro expression
parseDefmacro :: SExpr -> Lisp (Maybe (String, Macro))
parseDefmacro (SList p (SAtom defmacroPoint (ASymbol "defmacro"):name:lambdaList:body))
  | not $ isSymbol name = reportE (point name) "string expected"
  | otherwise           = do
      prototype <- parseLambdaList lambdaList
      currentE <- get
      return $ Just (fromSymbol name, Macro p currentE prototype body)
parseDefmacro (SList p (SAtom _ (ASymbol "defmacro"):_)) = reportE p "at least two arguments required"
parseDefmacro _                                          = return Nothing

-- | binds arguments to a procedure
bind :: Procedure -> [SExpr] -> Lisp Procedure
bind (UserDefined scope prototype@(Prototype _ _ Nothing) sexprs bound) args =
  return $ UserDefined scope prototype sexprs (bound ++ args)
bind (UserDefined scope prototype@(Prototype argNames optNames _) sexprs bound) args
  | length args + length args > length argNames + length optNames = reportE' "too many arguments"
  | otherwise                                                     =
      return $ UserDefined scope prototype sexprs (bound ++ args)
bind (BuiltIn name (Just argsCount) f bound) args
  | argsCount < (length bound + length args) = reportE' "too many arguments"
  | otherwise                                = return $ BuiltIn name (Just argsCount) f (bound ++ args)
bind (BuiltIn name Nothing f bound) args = return $ BuiltIn name Nothing f (bound ++ args)
bind (SpecialOp name (Just argsCount) f bound) args
  | argsCount < (length bound + length args) = reportE' "too many arguments"
  | otherwise                                 = return $ SpecialOp name (Just argsCount) f (bound ++ args)
bind (SpecialOp name Nothing f bound) args = return $ SpecialOp name Nothing f (bound ++ args)

-- | calls a procedure or special operator
call :: Point -> Procedure -> [SExpr] -> Lisp SExpr
call p c args = do
  exp <- call' c args
  return $ setPoint exp p
    where call' :: Procedure -> [SExpr] -> Lisp SExpr
          call' (UserDefined localE prototype sexprs bound) args = do
            previous <- get
            put localE
            argBindings <- bindArgs prototype (bound ++ args)
            modify (lappend argBindings)
            result <- evalScope sexprs
            put previous
            return result
          call' (BuiltIn name _ f bound) args = f (bound ++ args)
          call' (SpecialOp name _ f bound) args = f (bound ++ args)
