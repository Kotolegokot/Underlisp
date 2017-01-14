{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Evaluator where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (elemIndices, delete)
import Control.Monad (foldM, void)
import Prototype
import Point
import Exception
import Base

-- | evaluates a module with given environment and returns nothing
evaluateProgram :: Env -> [String] -> [SExpr] -> IO ()
evaluateProgram e args body = void $ expandAndEvalScope (setArgs e args) body

-- | evaluates a module with given environment
evaluateModule :: Env -> [SExpr] -> IO (Map String EnvItem)
evaluateModule e body = do
  (e, _) <- expandAndEvalScope e body
  return $ lexical e

bindArgs :: Prototype -> [SExpr] -> Map String EnvItem
bindArgs (Prototype argNames False) args
  | length argNames > length args = reportUndef "too little arguments"
  | length argNames < length args = reportUndef "too many arguments"
  | otherwise                     = Map.fromList (zip argNames $ map EnvSExpr args)
bindArgs (Prototype argNames True) args
  | length argNames - 1 > length args = reportUndef "too little arguments"
  | otherwise                         = let (left, right) = splitAt (length argNames - 1) args
                                            args'         = left ++ [list right]
                                        in Map.fromList (zip argNames $ map EnvSExpr args')

-- | takes an s-list of the form (arg1 arg2... [&rst argLast])
-- | and constructs a Prototype
parseLambdaList :: SExpr -> Prototype
parseLambdaList (SList p lambdaList)
  | not $ all isSymbol lambdaList  = report p "all items in a lambda list must be symbols"
  | length ixs > 1                 = report p "more than one &rest in a lambda list is forbidden"
  | rest && ix /= count - 2        = report p "&rest must be last but one"
  | otherwise                      = if rest
                                     then Prototype (delete "&rest" . map fromSymbol $ lambdaList) rest
                                     else Prototype (map fromSymbol $ lambdaList) rest
  where ixs   = elemIndices (symbol "&rest") lambdaList
        ix    = head ixs
        rest  = length ixs == 1
        count = length lambdaList
parseLambdaList _ = reportUndef "lambda list must be a list"

-- | evaluates a lexical scope
evalScope :: Env -> [SExpr] -> IO (Env, SExpr)
evalScope e = foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e, nil)

-- | evaluates a lexical scope as if it is the same
-- | lexical level
evalScopeInterpolated :: Env -> [SExpr] -> IO (Env, SExpr)
evalScopeInterpolated e sexprs = do
  let (e', sexprs') = collectMacros e sexprs
  sexprs'' <- expandMacros e' sexprs'
  foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e', nil) sexprs''

-- | evaluates an s-expression
eval :: Env -> SExpr -> IO (Env, SExpr)
eval e (SList _ (first:args))  = do
  (_, first') <- eval e first
  rethrow (\le -> if lePoint le == Undefined
                  then le { lePoint = point first }
                  else le) $
    if isProcedure first'
    then eval' $ fromProcedure first'
    else report (point first) $ "unable to execute s-expression: '" ++ show first' ++ "'"
  where eval' c | isUserDefined c || isBuiltIn c = do
                    pairs <- mapM (eval e) args
                    call (point first) e c (map snd pairs)
                | isSpecialOp c     = call (point first) e c args
eval e (SAtom p (ASymbol "_")) = report p "addressing '_' is forbidden"
eval e (SAtom p (ASymbol sym)) = case envLookup sym e of
  Just (EnvSExpr s) -> return (e, setPoint s p)
  _                 -> report p $ "undefined identificator '" ++ sym ++ "'"
eval e sexpr                   = return (e, sexpr)

---- applicable
class Applicable a where
  bind :: a -> [SExpr] -> a
  call :: Point -> Env -> a -> [SExpr] -> IO (Env, SExpr)
---- applicable

callMacro :: Env -> Macro -> [SExpr] -> IO SExpr
callMacro e (Macro p localE prototype sexprs bound) args = do
  let argBindings = bindArgs prototype (bound ++ args)
  (_, evaluated) <- expandAndEvalScope (lappend (pass localE) argBindings) sexprs
  return $ setPoint evaluated p

-- | expands macros and evaluates a scope
expandAndEvalScope :: Env -> [SExpr] -> IO (Env, SExpr)
expandAndEvalScope e sexprs = do
  let (e', sexprs') = collectMacros (pass e) sexprs
  sexprs'' <- expandMacros e' sexprs'
  foldM (\(prevE, _) sexpr -> eval prevE sexpr) (e', nil) sexprs''

-- | takes a scope and evaluates all top-level
-- | defmacros in it
collectMacros :: Env -> [SExpr] -> (Env, [SExpr])
collectMacros e xs = foldl (\(accE, accSexprs) sexpr -> case parseDefmacro accE sexpr of
                               Just (name, macro) -> (linsert name (EnvMacro macro) accE, accSexprs)
                               Nothing            -> (accE, accSexprs ++ [sexpr]))
                     (e, [])
                     xs

-- | expands all top-level macros
expandMacros :: Env -> [SExpr] -> IO [SExpr]
expandMacros e (x:xs) = do
  expr <- expandMacro e x
  rest <- expandMacros e xs
  return (expr : rest)
expandMacros _ []     = return []

data EMState = Default | Backquote

-- | expands one macro expression recursively

expandMacro :: Env -> SExpr -> IO SExpr
expandMacro = expandMacro' Default

expandMacro' :: EMState -> Env -> SExpr -> IO SExpr
expandMacro' Default e l@(SList p (first@(SAtom _ (ASymbol sym)):rest))
  | sym == "quote"       = return l
  | sym == "backquote"   = do
      rest' <- mapM (expandMacro' Backquote e) rest
      return $ SList p (first:rest')
  | sym == "interpolate" = reportCmd p "interpolate" "calling out of backquote"
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
parseDefmacro :: Env -> SExpr -> Maybe (String, Macro)
parseDefmacro e (SList p (SAtom defmacroPoint (ASymbol "defmacro"):name:lambdaList:body))
  | not $ isSymbol name = reportCmd (point name) "defmacro" "string expected"
  | otherwise           = return (fromSymbol name, Macro p e prototype body [])
  where prototype = parseLambdaList lambdaList

parseDefmacro _ (SList p (SAtom _ (ASymbol "defmacro"):_)) = reportCmd p "defmacro" "at least two arguments required"
parseDefmacro _ _                                          = Nothing

instance Applicable Procedure where
  bind (UserDefined scope prototype@(Prototype argNames rest) sexprs bound) args
    | rest && length argNames < (length bound + length args) = reportUndef "too many arguments"
    | otherwise                                              = UserDefined scope prototype sexprs (bound ++ args)
  bind (BuiltIn name (Just argsCount) f bound) args
    | argsCount < (length bound + length args) = reportUndef "too many arguments"
    | otherwise                                 = BuiltIn name (Just argsCount) f (bound ++ args)
  bind (BuiltIn name Nothing f bound) args = BuiltIn name Nothing f (bound ++ args)
  bind (SpecialOp name (Just argsCount) f bound) args
    | argsCount < (length bound + length args) = reportUndef "too many arguments"
    | otherwise                                 = SpecialOp name (Just argsCount) f (bound ++ args)
  bind (SpecialOp name Nothing f bound) args = SpecialOp name Nothing f (bound ++ args)

  call p e c args = do
    (e', expr) <- call' eval evalScope e c args
    return (e', setPoint expr p)
      where call' eval evalScope e (UserDefined localE prototype sexprs bound) args = do
              let argBindings = bindArgs prototype (bound ++ args)
              (_, expr) <- evalScope (lappend localE argBindings) sexprs
              return (e, expr)

            call' eval evalScope e (BuiltIn name _ f bound) args = rethrow
              (\le -> if null $ leCmd le then le { leCmd = name } else le) $ do
                result <- f (bound ++ args)
                return (e, result)

            call' eval evalScope e (SpecialOp name _ f bound) args = rethrow
              (\le -> if null $ leCmd le then le { leCmd = name } else le) $ do
                (e', expr) <- f e (bound ++ args)
                return (e', expr)

assureStrings :: [SExpr] -> [String]
assureStrings = foldl (\acc s -> if isString s
                                 then acc ++ [fromString s]
                                 else report (point s) "string expected")
                []
