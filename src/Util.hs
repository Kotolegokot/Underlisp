{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Util where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (elemIndices, delete)
import Prototype
import Point
import Exception
import Base

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

---- applicable
class Applicable a where
  bind :: a -> [SExpr] -> a
  call :: Point -> Eval -> EvalScope -> Env -> a -> [SExpr] -> IO (Env, SExpr)
---- applicable

instance Applicable Macro where
  bind (Macro p scope prototype@(Prototype argNames rest) sexprs bound) args
    | rest && length argNames < (length bound + length args) = reportUndef "too many arguments"
    | otherwise                                              = Macro p scope prototype sexprs (bound ++ args)
  call p eval evalScope e c args = do
    (e', expr) <- call' eval evalScope e c args
    return (e', setPoint expr p)
      where call' eval evalScope e (Macro _ localE prototype sexprs bound) args = do
              let argBindings = bindArgs prototype (bound ++ args)
              (_, expr) <- evalScope (lappend localE argBindings) sexprs
              (e', expr') <- eval e expr
              return (e', expr')

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

  call p eval evalScope e c args = do
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
                (e', expr) <- f eval evalScope e (bound ++ args)
                return (e', expr)

assureStrings :: [SExpr] -> [String]
assureStrings = foldl (\acc s -> if isString s
                                 then acc ++ [fromString s]
                                 else report (point s) "string expected")
                []
