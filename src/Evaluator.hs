module Evaluator where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromJust)
import Control.Monad (foldM, void)
import Control.Monad.Reader
import Prototype
import Point
import Base

-- | evaluates a module with the given environment and returns nothing
evaluateProgram :: Env -> [String] -> [SExpr] -> Eval ()
evaluateProgram e args body = void $ expandAndEvalScope (setArgs e args) body

-- | evaluates a module with the given environment
evaluateModule :: Env -> [SExpr] -> Eval (Map String EnvItem)
evaluateModule e body = do
  (e, _) <- expandAndEvalScope e body
  return $ lexical e

-- | expands macros and evaluates a scope
expandAndEvalScope :: Env -> [SExpr] -> Eval (Env, SExpr)
expandAndEvalScope e sexprs = do
  (e', sexprs') <- collectMacros (pass e) sexprs
  sexprs'' <- expandMacros e' sexprs'
  foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e', nil) sexprs''

-- | expands macros and evaluates a scope as if
-- | it is at the same lexical level
expandAndEvalScopeInterpolated :: Env -> [SExpr] -> Eval (Env, SExpr)
expandAndEvalScopeInterpolated e sexprs = do
  (e', sexprs') <- collectMacros e sexprs
  sexprs'' <- expandMacros e' sexprs'
  foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e', nil) sexprs''

-- | evaluates a lexical scope
evalScope :: Env -> [SExpr] -> Eval (Env, SExpr)
evalScope e = foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e, nil)

-- | evaluates an s-expression writing into call stack
-- | if any function is invoked
eval :: Env -> SExpr -> Eval (Env, SExpr)
eval e l@(SList p (sFirst:args)) = do
  (_, first) <- eval e sFirst
  rethrow (\le -> if lePoint le == Undefined
                  then le { lePoint = point sFirst }
                  else le) $
    if isProcedure first
    then eval' (fromProcedure first)
    else report (point sFirst) $ "unable to execute s-expression: '" ++ show first ++ "'"
  where eval' :: Procedure -> Eval (Env, SExpr)
        eval' pr
          | isUserDefined pr || isBuiltIn pr = do
              args' <- mapM (return . snd <=< eval e) args
              add (Call p $ SList p (procedure pr:args')) $ call (point sFirst) e pr args'
          | otherwise                        = -- isSpecialOp
              add (Call p $ SList p (procedure pr:args)) $ call (point sFirst) e pr args
eval e (SAtom p (ASymbol "_"))   = report p "addressing '_' is forbidden"
eval e (SAtom p (ASymbol sym))   = case envLookup sym e of
  Just (EnvSExpr s) -> return (e, setPoint s p)
  _                 -> report p $ "undefined identificator '" ++ sym ++ "'"
eval e other                     = return (e, other)

-- | Create bindings from a function prototype and
-- | and the actual arguments
bindArgs :: Prototype -> [SExpr] -> Eval (Map String EnvItem)
bindArgs (Prototype argNames optNames Nothing) args
  | length args > l1 + l2 = reportUndef "too many arguments"
  | length args < l1      = reportUndef "too little arguments"
  | otherwise             = return $ Map.fromList $ zip (argNames ++ optNames) (map EnvSExpr $ args ++ repeat nil)
    where l1 = length argNames
          l2 = length optNames
bindArgs (Prototype argNames optNames rest) args
  | l < l1      = reportUndef "too little arguments"
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
parseLambdaList :: SExpr -> Eval Prototype
parseLambdaList exp = parseLambdaList' PLLNone (Prototype [] [] Nothing) =<< getList exp
  where parseLambdaList' :: PLLStatus -> Prototype -> [SExpr] -> Eval Prototype
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
            "&optional" -> report (point x) "more than one &optional in a lambda list"
            "&rest"     -> parseLambdaList' PLLRest     prototype xs
            "&body"     -> parseLambdaList' PLLBody     prototype xs
            other       -> parseLambdaList' PLLOptional prototype { getOptional =
                                                                      getOptional prototype ++ [other] } xs
        parseLambdaList' PLLOptional prototype []     = return prototype

        parseLambdaList' PLLRest prototype (x:xs) = do
          x' <- getSymbol x
          case x' of
            "&optional" -> report (point x) "no &optional after &rest expected"
            "&rest"     -> report (point x) "no &rest after &rest expected"
            "&body"     -> report (point x) "no &body after &rest expected"
            other       -> parseLambdaList' PLLEnd prototype { getRest = Just (Rest other) } xs
        parseLambdaList' PLLRest _         []     = reportUndef "a symbol after &rest expected"

        parseLambdaList' PLLBody prototype (x:xs) = do
          x' <- getSymbol x
          case x' of
            "&optional" -> report (point x) "no &optional after &body expected"
            "&rest"     -> report (point x) "no &rest after &body expected"
            "&body"     -> report (point x) "no &body after &body expected"
            other       -> parseLambdaList' PLLEnd prototype { getRest = Just (Body other) } xs
        parseLambdaList' PLLBody _         []     = reportUndef "a symbol after &body expected"

        parseLambdaList' PLLEnd prototype []      = return prototype
        parseLambdaList' PLLEnd _         (x:xs)  = report (point x) "nothing expected at the end of the lambda list"

-- | invokes a macro with the given environment and arguments
callMacro :: Env -> Macro -> [SExpr] -> Eval SExpr
callMacro e (Macro p localE prototype sexprs) args = do
  argBindings <- bindArgs prototype args
  (_, evaluated) <- expandAndEvalScope (lappend (pass localE) argBindings) sexprs
  return $ setPoint evaluated p

-- | takes a scope and evaluates all top-level
-- | defmacros in it
collectMacros :: Env -> [SExpr] -> Eval (Env, [SExpr])
collectMacros e = foldM (\(accE, accSexprs) sexpr -> do
                            defmacro <- parseDefmacro accE sexpr
                            return $ case defmacro of
                              Just (name, macro) -> (linsert name (EnvMacro macro) accE, accSexprs)
                              Nothing            -> (accE, accSexprs ++ [sexpr]))
                  (e, [])

-- | expands all top-level macros
expandMacros :: Env -> [SExpr] -> Eval [SExpr]
expandMacros e (x:xs) = do
  expr <- expandMacro e x
  rest <- expandMacros e xs
  return (expr : rest)
expandMacros _ []     = return []

data EMState = Default | Backquote

-- | expands one macro expression recursively
expandMacro :: Env -> SExpr -> Eval SExpr
expandMacro = expandMacro' Default
  where expandMacro' :: EMState -> Env -> SExpr -> Eval SExpr
        expandMacro' Default e l@(SList p (first@(SAtom _ (ASymbol sym)):rest))
          | sym == "quote"       = return l
          | sym == "backquote"   = do
              rest' <- mapM (expandMacro' Backquote e) rest
              return $ SList p (first:rest')
          | sym == "interpolate" = report p "calling out of backquote"
          | otherwise = case lookupMacro (fromSymbol first) e of
              Just m  -> do
                expr <- callMacro e m rest
                expandMacro' Default e expr
              Nothing -> do
                list' <- mapM (expandMacro' Default e) (first:rest)
                return $ SList p list'
        expandMacro' Default e l@(SList p (first:rest)) = do
          first' <- expandMacro' Default e first
          if isSymbol first'
            then expandMacro' Default e $ SList p (first':rest)
            else do
              rest' <- mapM (expandMacro' Default e) rest
              return $ SList p (first':rest')
        expandMacro' Default _ other                    = return other
        expandMacro' Backquote e l@(SList p (first@(SAtom _ (ASymbol sym)):rest))
          | sym == "interpolate" = do
              rest' <- mapM (expandMacro' Default e) rest
              return $ SList p (first:rest')
          | otherwise            = return l
        expandMacro' Backquote _ other = return other

-- | parses a defmacro expression
parseDefmacro :: Env -> SExpr -> Eval (Maybe (String, Macro))
parseDefmacro e (SList p (SAtom defmacroPoint (ASymbol "defmacro"):name:lambdaList:body))
  | not $ isSymbol name = report (point name) "string expected"
  | otherwise           = do
      prototype <- parseLambdaList lambdaList
      return $ Just (fromSymbol name, Macro p e prototype body)
parseDefmacro _ (SList p (SAtom _ (ASymbol "defmacro"):_)) = report p "at least two arguments required"
parseDefmacro _ _                                          = return Nothing

-- | binds arguments to a procedure
bind :: Procedure -> [SExpr] -> Eval Procedure
bind (UserDefined scope prototype@(Prototype _ _ Nothing) sexprs bound) args =
  return $ UserDefined scope prototype sexprs (bound ++ args)
bind (UserDefined scope prototype@(Prototype argNames optNames _) sexprs bound) args
  | length args + length args > length argNames + length optNames = reportUndef "too many arguments"
  | otherwise                                                     =
      return $ UserDefined scope prototype sexprs (bound ++ args)
bind (BuiltIn name (Just argsCount) f bound) args
  | argsCount < (length bound + length args) = reportUndef "too many arguments"
  | otherwise                                = return $ BuiltIn name (Just argsCount) f (bound ++ args)
bind (BuiltIn name Nothing f bound) args = return $ BuiltIn name Nothing f (bound ++ args)
bind (SpecialOp name (Just argsCount) f bound) args
  | argsCount < (length bound + length args) = reportUndef "too many arguments"
  | otherwise                                 = return $ SpecialOp name (Just argsCount) f (bound ++ args)
bind (SpecialOp name Nothing f bound) args = return $ SpecialOp name Nothing f (bound ++ args)

-- | calls a procedure or special operator
call :: Point -> Env -> Procedure -> [SExpr] -> Eval (Env, SExpr)
call p e c args = do
  (e', exp) <- call' e c args
  return (e', setPoint exp p)
    where call' :: Env -> Procedure -> [SExpr] -> Eval (Env, SExpr)
          call' e (UserDefined localE prototype sexprs bound) args = do
            argBindings <- bindArgs prototype (bound ++ args)
            (_, expr) <- evalScope (lappend localE argBindings) sexprs
            return (e, expr)
          call' e (BuiltIn name _ f bound) args = do
            result <- f (bound ++ args)
            return (e, result)
          call' e (SpecialOp name _ f bound) args = do
            (e', expr) <- f e (bound ++ args)
            return (e', expr)
