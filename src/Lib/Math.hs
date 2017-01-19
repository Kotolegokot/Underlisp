module Lib.Math (builtinFunctions
                ,specialOperators) where

import Numeric.Special.Trigonometric
import Control.Monad (liftM2)
import Base

default (Int)

biSum :: [SExpr] -> Eval SExpr
biSum sexprs = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int . sum . fmap fromInt $ sexprs
    NTFloat -> return . float . sum . fmap fromNumber $ sexprs

biSubstract :: [SExpr] -> Eval SExpr
biSubstract sexprs@[num1, num2] = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int $ fromInt num1 - fromInt num2
    NTFloat -> return . float $ fromNumber num1 - fromNumber num2
biSubstract _                   = reportUndef "two arguments required"

biProduct :: [SExpr] -> Eval SExpr
biProduct sexprs = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int . product . fmap fromInt $ sexprs
    NTFloat -> return . float . product . fmap fromNumber $ sexprs

biDivide :: [SExpr] -> Eval SExpr
biDivide sexprs@[num1, num2] = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int $ fromInt num1 `div` fromInt num2
    NTFloat -> return . float $ fromNumber num1 / fromNumber num2
biDivide _                   = reportUndef "two arguments required"

data NumType = NTInt | NTFloat
numArgs :: [SExpr] -> Eval NumType
numArgs sexprs = numArgs' sexprs NTInt
  where numArgs' (x:xs) NTInt
          | isInt x    = numArgs' xs NTInt
          | isFloat x  = numArgs' xs NTFloat
          | otherwise  = report (point x) "float or int expected"
        numArgs' (x:xs) NTFloat
          | isNumber x  = numArgs' xs NTFloat
          | otherwise   = report (point x) "float or int expected"
        numArgs' [] return_type = return return_type

biFloat :: [SExpr] -> Eval SExpr
biFloat [exp] = float <$> getNumber exp
biFloat _     = reportUndef "just one argument requried"

biExp :: [SExpr] -> Eval SExpr
biExp [expr] = float . exp <$> getNumber expr
biExp _      = reportUndef "just one argument required"

biLn :: [SExpr] -> Eval SExpr
biLn [exp] = float . log <$> getNumber exp
biLn _ = reportUndef "just one argument required"

biPower :: [SExpr] -> Eval SExpr
biPower nums@[num1, num2] = do
  t <- numArgs nums
  case t of
    NTInt   -> return . int $ fromInt num1 ^ fromInt num2
    NTFloat -> return . float $ fromNumber num1 ** fromNumber num2
biPower _ = reportUndef "two arguments required"

biSin :: [SExpr] -> Eval SExpr
biSin [exp] = float . sin <$> getNumber exp
biSin _     = reportUndef "just one argument required"

biCos :: [SExpr] -> Eval SExpr
biCos [exp] = float . cos <$> getNumber exp
biCos _     = reportUndef "just one argument required"

biASin :: [SExpr] -> Eval SExpr
biASin [exp] = float . asin <$> getNumber exp
biASin _     = reportUndef "just one argument required"

biACos :: [SExpr] -> Eval SExpr
biACos [exp] = float . acos <$> getNumber exp
biACos _     = reportUndef "just one argument required"

biATan :: [SExpr] -> Eval SExpr
biATan [exp] = float . atan <$> getNumber exp
biATan _     = reportUndef "just one argument required"

biACot :: [SExpr] -> Eval SExpr
biACot [exp] = float . acot <$> getNumber exp
biACot _     = reportUndef "just one argument required"

biSinH :: [SExpr] -> Eval SExpr
biSinH [exp] = float . sinh <$> getNumber exp
biSinH _     = reportUndef "just one argument required"

biCosH :: [SExpr] -> Eval SExpr
biCosH [exp] = float . cosh <$> getNumber exp
biCosH _     = reportUndef "just one argument required"

biASinH :: [SExpr] -> Eval SExpr
biASinH [exp] = float . asinh <$> getNumber exp
biASinH _     = reportUndef "just one argument required"

biACosH :: [SExpr] -> Eval SExpr
biACosH [exp] = float . acosh <$> getNumber exp
biACosH _     = reportUndef "just one argument required"

biATanH :: [SExpr] -> Eval SExpr
biATanH [exp] = float . atanh <$> getNumber exp
biATanH _     = reportUndef "just one argument required"

biACotH :: [SExpr] -> Eval SExpr
biACotH [exp] = float . acoth <$> getNumber exp
biACotH _     = reportUndef "just one argument required"

biTruncate :: [SExpr] -> Eval SExpr
biTruncate [exp] = int . truncate <$> getNumber exp
biTruncate _     = reportUndef "just one argument required"

biRound :: [SExpr] -> Eval SExpr
biRound [exp] = int . round <$> getNumber exp
biRound _     = reportUndef "just one argument required"

biCeiling :: [SExpr] -> Eval SExpr
biCeiling [exp] = int . ceiling <$> getNumber exp
biCeiling _     = reportUndef "just one argument required"

biFloor :: [SExpr] -> Eval SExpr
biFloor [exp] = int . floor <$> getNumber exp
biFloor _     = reportUndef "just one argument required"

biIsNan :: [SExpr] -> Eval SExpr
biIsNan [exp] = bool . isNaN <$> getFloat exp
biIsNan _     = reportUndef "just one argument required"

biIsInfinite :: [SExpr] -> Eval SExpr
biIsInfinite [exp] = bool . isInfinite <$> getFloat exp
biIsInfinite _     = reportUndef "just one argument required"

biIsDenormalized :: [SExpr] -> Eval SExpr
biIsDenormalized [exp] = bool . isDenormalized <$> getFloat exp
biIsDenormalized _     = reportUndef "just one argument required"

biIsNegativeZero :: [SExpr] -> Eval SExpr
biIsNegativeZero [exp] = bool . isNegativeZero <$> getFloat exp
biIsNegativeZero _     = reportUndef "just one argument required"

biIsIEEE :: [SExpr] -> Eval SExpr
biIsIEEE [exp] = bool . isIEEE <$> getFloat exp
biIsIEEE _     = reportUndef "just one argument required"

biQuotRem :: [SExpr] -> Eval SExpr
biQuotRem [exp1, exp2] = do
  (quot, rem) <- liftM2 quotRem (getInt exp1) (getInt exp2)
  return $ list [int quot, int rem]
biQuotRem _            = reportUndef "two arguments required"

biDivMod :: [SExpr] -> Eval SExpr
biDivMod [exp1, exp2] = do
  (div, mod) <- liftM2 divMod (getInt exp1) (getInt exp2)
  return $ list [int div, int mod]
biDivMod _            = reportUndef "two arguments required"

builtinFunctions = [("+",              Nothing, biSum)
                   ,("-",              Just 2,  biSubstract)
                   ,("*",              Nothing, biProduct)
                   ,("/",              Just 2,  biDivide)
                   ,("float",          Just 1,  biFloat)
                   ,("exp",            Just 1,  biExp)
                   ,("ln",             Just 1,  biLn)
                   ,("^",              Just 2,  biPower)
                   ,("sin",            Just 1,  biSin)
                   ,("cos",            Just 1,  biCos)
                   ,("asin",           Just 1,  biASin)
                   ,("acos",           Just 1,  biACos)
                   ,("atan",           Just 1,  biATan)
                   ,("acot",           Just 1,  biACot)
                   ,("sinh",           Just 1,  biSinH)
                   ,("cosh",           Just 1,  biCosH)
                   ,("truncate",       Just 1,  biTruncate)
                   ,("round",          Just 1,  biRound)
                   ,("ceiling",        Just 1,  biCeiling)
                   ,("floor",          Just 1,  biFloor)
                   ,("nan?",           Just 1,  biIsNan)
                   ,("infinite?",      Just 1,  biIsInfinite)
                   ,("denormalized?",  Just 1,  biIsDenormalized)
                   ,("negative-zero?", Just 1,  biIsNegativeZero)
                   ,("IEEE?",          Just 1,  biIsIEEE)
                   ,("quot-rem",       Just 1,  biQuotRem)
                   ,("div-mod",        Just 1,  biDivMod)]

specialOperators = []

