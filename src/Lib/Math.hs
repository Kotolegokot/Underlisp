module Lib.Math (specialOperators) where

import Numeric.Special.Trigonometric
import Control.Monad (liftM2)
import Data.IORef

-- local modules
import Base
import Evaluator

default (Int)

biSum :: IORef Scope -> [SExpr] -> EvalM SExpr
biSum _ sexprs = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int . sum . fmap fromInt $ sexprs
    NTFloat -> return . float . sum . fmap fromNumber $ sexprs

biSubstract :: IORef Scope -> [SExpr] -> EvalM SExpr
biSubstract _ sexprs@[num1, num2] = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int $ fromInt num1 - fromInt num2
    NTFloat -> return . float $ fromNumber num1 - fromNumber num2
biSubstract _ _                   = reportE' "two arguments required"

biProduct :: IORef Scope -> [SExpr] -> EvalM SExpr
biProduct _ sexprs = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int . product . fmap fromInt $ sexprs
    NTFloat -> return . float . product . fmap fromNumber $ sexprs

biDivide :: IORef Scope -> [SExpr] -> EvalM SExpr
biDivide _ sexprs@[num1, num2] = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int $ fromInt num1 `div` fromInt num2
    NTFloat -> return . float $ fromNumber num1 / fromNumber num2
biDivide _ _                   = reportE' "two arguments required"

data NumType = NTInt | NTFloat
numArgs :: [SExpr] -> EvalM NumType
numArgs sexprs = numArgs' sexprs NTInt
  where numArgs' (x:xs) NTInt
          | isInt x    = numArgs' xs NTInt
          | isFloat x  = numArgs' xs NTFloat
          | otherwise  = reportE (point x) "float or int expected"
        numArgs' (x:xs) NTFloat
          | isNumber x  = numArgs' xs NTFloat
          | otherwise   = reportE (point x) "float or int expected"
        numArgs' [] return_type = return return_type

biFloat :: IORef Scope -> [SExpr] -> EvalM SExpr
biFloat _ [exp] = float <$> getNumber exp
biFloat _ _     = reportE' "just one argument requried"

unaryFunction :: (SExpr -> EvalM SExpr) -> IORef Scope -> [SExpr] -> EvalM SExpr
unaryFunction f _ [exp] = f exp
unaryFunction _ _ _     = reportE' "just one argument requried"

biPower :: IORef Scope -> [SExpr] -> EvalM SExpr
biPower _ nums@[num1, num2] = do
  t <- numArgs nums
  case t of
    NTInt   -> return . int $ fromInt num1 ^ fromInt num2
    NTFloat -> return . float $ fromNumber num1 ** fromNumber num2
biPower _ _ = reportE' "two arguments required"

biQuotRem :: IORef Scope -> [SExpr] -> EvalM SExpr
biQuotRem _ [exp1, exp2] = do
  (quot, rem) <- liftM2 quotRem (getInt exp1) (getInt exp2)
  return $ list [int quot, int rem]
biQuotRem _ _            = reportE' "two arguments required"

biDivMod :: IORef Scope -> [SExpr] -> EvalM SExpr
biDivMod _ [exp1, exp2] = do
  (div, mod) <- liftM2 divMod (getInt exp1) (getInt exp2)
  return $ list [int div, int mod]
biDivMod _ _            = reportE' "two arguments required"

specialOperators = [("+",              Nothing, withEvaluatedArgs biSum)
                   ,("-",              Just 2,  withEvaluatedArgs biSubstract)
                   ,("*",              Nothing, withEvaluatedArgs biProduct)
                   ,("/",              Just 2,  withEvaluatedArgs biDivide)
                   ,("float",          Just 1,  withEvaluatedArgs biFloat)
                   ,("exp",            Just 1,  withEvaluatedArgs $ unaryFunction (fmap (float . exp) . getNumber))
                   ,("ln",             Just 1,  withEvaluatedArgs $ unaryFunction (fmap (float . log) . getNumber))
                   ,("^",              Just 2,  withEvaluatedArgs biPower)
                   ,("sin",            Just 1,  withEvaluatedArgs $ unaryFunction (fmap (float . sin           ) .  getNumber))
                   ,("cos",            Just 1,  withEvaluatedArgs $ unaryFunction (fmap (float . cos           ) .  getNumber))
                   ,("asin",           Just 1,  withEvaluatedArgs $ unaryFunction (fmap (float . asin          ) .  getNumber))
                   ,("acos",           Just 1,  withEvaluatedArgs $ unaryFunction (fmap (float . acos          ) .  getNumber))
                   ,("atan",           Just 1,  withEvaluatedArgs $ unaryFunction (fmap (float . atan          ) .  getNumber))
                   ,("acot",           Just 1,  withEvaluatedArgs $ unaryFunction (fmap (float . acot          ) .  getNumber))
                   ,("sinh",           Just 1,  withEvaluatedArgs $ unaryFunction (fmap (float . sinh          ) .  getNumber))
                   ,("cosh",           Just 1,  withEvaluatedArgs $ unaryFunction (fmap (float . cosh          ) .  getNumber))
                   ,("truncate",       Just 1,  withEvaluatedArgs $ unaryFunction (fmap (int   . truncate      ) .  getNumber))
                   ,("round",          Just 1,  withEvaluatedArgs $ unaryFunction (fmap (int   . round         ) .  getNumber))
                   ,("ceiling",        Just 1,  withEvaluatedArgs $ unaryFunction (fmap (int   . ceiling       ) .  getNumber))
                   ,("floor",          Just 1,  withEvaluatedArgs $ unaryFunction (fmap (int   . floor         ) .  getNumber))
                   ,("nan?",           Just 1,  withEvaluatedArgs $ unaryFunction (fmap (bool  . isNaN         ) .  getFloat))
                   ,("infinite?",      Just 1,  withEvaluatedArgs $ unaryFunction (fmap (bool  . isInfinite    ) .  getFloat))
                   ,("denormalized?",  Just 1,  withEvaluatedArgs $ unaryFunction (fmap (bool  . isDenormalized) .  getFloat))
                   ,("negative-zero?", Just 1,  withEvaluatedArgs $ unaryFunction (fmap (bool  . isNegativeZero) .  getFloat))
                   ,("IEEE?",          Just 1,  withEvaluatedArgs $ unaryFunction (fmap (bool  . isIEEE        ) .  getFloat))
                   ,("quot-rem",       Just 1,  withEvaluatedArgs biQuotRem)
                   ,("div-mod",        Just 1,  withEvaluatedArgs biDivMod)]
