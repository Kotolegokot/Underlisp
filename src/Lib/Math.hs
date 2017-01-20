module Lib.Math (builtinFunctions
                ,specialOperators) where

import Numeric.Special.Trigonometric
import Control.Monad (liftM2)
import Base

default (Int)

biSum :: [SExpr] -> Lisp SExpr
biSum sexprs = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int . sum . fmap fromInt $ sexprs
    NTFloat -> return . float . sum . fmap fromNumber $ sexprs

biSubstract :: [SExpr] -> Lisp SExpr
biSubstract sexprs@[num1, num2] = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int $ fromInt num1 - fromInt num2
    NTFloat -> return . float $ fromNumber num1 - fromNumber num2
biSubstract _                   = reportUndef "two arguments required"

biProduct :: [SExpr] -> Lisp SExpr
biProduct sexprs = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int . product . fmap fromInt $ sexprs
    NTFloat -> return . float . product . fmap fromNumber $ sexprs

biDivide :: [SExpr] -> Lisp SExpr
biDivide sexprs@[num1, num2] = do
  t <- numArgs sexprs
  case t of
    NTInt   -> return . int $ fromInt num1 `div` fromInt num2
    NTFloat -> return . float $ fromNumber num1 / fromNumber num2
biDivide _                   = reportUndef "two arguments required"

data NumType = NTInt | NTFloat
numArgs :: [SExpr] -> Lisp NumType
numArgs sexprs = numArgs' sexprs NTInt
  where numArgs' (x:xs) NTInt
          | isInt x    = numArgs' xs NTInt
          | isFloat x  = numArgs' xs NTFloat
          | otherwise  = report (point x) "float or int expected"
        numArgs' (x:xs) NTFloat
          | isNumber x  = numArgs' xs NTFloat
          | otherwise   = report (point x) "float or int expected"
        numArgs' [] return_type = return return_type

biFloat :: [SExpr] -> Lisp SExpr
biFloat [exp] = float <$> getNumber exp
biFloat _     = reportUndef "just one argument requried"

biExp :: [SExpr] -> Lisp SExpr
biExp [expr] = float . exp <$> getNumber expr
biExp _      = reportUndef "just one argument required"

biLn :: [SExpr] -> Lisp SExpr
biLn [exp] = float . log <$> getNumber exp
biLn _ = reportUndef "just one argument required"

biPower :: [SExpr] -> Lisp SExpr
biPower nums@[num1, num2] = do
  t <- numArgs nums
  case t of
    NTInt   -> return . int $ fromInt num1 ^ fromInt num2
    NTFloat -> return . float $ fromNumber num1 ** fromNumber num2
biPower _ = reportUndef "two arguments required"

biSin :: [SExpr] -> Lisp SExpr
biSin [exp] = float . sin <$> getNumber exp
biSin _     = reportUndef "just one argument required"

biCos :: [SExpr] -> Lisp SExpr
biCos [exp] = float . cos <$> getNumber exp
biCos _     = reportUndef "just one argument required"

biASin :: [SExpr] -> Lisp SExpr
biASin [exp] = float . asin <$> getNumber exp
biASin _     = reportUndef "just one argument required"

biACos :: [SExpr] -> Lisp SExpr
biACos [exp] = float . acos <$> getNumber exp
biACos _     = reportUndef "just one argument required"

biATan :: [SExpr] -> Lisp SExpr
biATan [exp] = float . atan <$> getNumber exp
biATan _     = reportUndef "just one argument required"

biACot :: [SExpr] -> Lisp SExpr
biACot [exp] = float . acot <$> getNumber exp
biACot _     = reportUndef "just one argument required"

biSinH :: [SExpr] -> Lisp SExpr
biSinH [exp] = float . sinh <$> getNumber exp
biSinH _     = reportUndef "just one argument required"

biCosH :: [SExpr] -> Lisp SExpr
biCosH [exp] = float . cosh <$> getNumber exp
biCosH _     = reportUndef "just one argument required"

biASinH :: [SExpr] -> Lisp SExpr
biASinH [exp] = float . asinh <$> getNumber exp
biASinH _     = reportUndef "just one argument required"

biACosH :: [SExpr] -> Lisp SExpr
biACosH [exp] = float . acosh <$> getNumber exp
biACosH _     = reportUndef "just one argument required"

biATanH :: [SExpr] -> Lisp SExpr
biATanH [exp] = float . atanh <$> getNumber exp
biATanH _     = reportUndef "just one argument required"

biACotH :: [SExpr] -> Lisp SExpr
biACotH [exp] = float . acoth <$> getNumber exp
biACotH _     = reportUndef "just one argument required"

biTruncate :: [SExpr] -> Lisp SExpr
biTruncate [exp] = int . truncate <$> getNumber exp
biTruncate _     = reportUndef "just one argument required"

biRound :: [SExpr] -> Lisp SExpr
biRound [exp] = int . round <$> getNumber exp
biRound _     = reportUndef "just one argument required"

biCeiling :: [SExpr] -> Lisp SExpr
biCeiling [exp] = int . ceiling <$> getNumber exp
biCeiling _     = reportUndef "just one argument required"

biFloor :: [SExpr] -> Lisp SExpr
biFloor [exp] = int . floor <$> getNumber exp
biFloor _     = reportUndef "just one argument required"

biIsNan :: [SExpr] -> Lisp SExpr
biIsNan [exp] = bool . isNaN <$> getFloat exp
biIsNan _     = reportUndef "just one argument required"

biIsInfinite :: [SExpr] -> Lisp SExpr
biIsInfinite [exp] = bool . isInfinite <$> getFloat exp
biIsInfinite _     = reportUndef "just one argument required"

biIsDenormalized :: [SExpr] -> Lisp SExpr
biIsDenormalized [exp] = bool . isDenormalized <$> getFloat exp
biIsDenormalized _     = reportUndef "just one argument required"

biIsNegativeZero :: [SExpr] -> Lisp SExpr
biIsNegativeZero [exp] = bool . isNegativeZero <$> getFloat exp
biIsNegativeZero _     = reportUndef "just one argument required"

biIsIEEE :: [SExpr] -> Lisp SExpr
biIsIEEE [exp] = bool . isIEEE <$> getFloat exp
biIsIEEE _     = reportUndef "just one argument required"

biQuotRem :: [SExpr] -> Lisp SExpr
biQuotRem [exp1, exp2] = do
  (quot, rem) <- liftM2 quotRem (getInt exp1) (getInt exp2)
  return $ list [int quot, int rem]
biQuotRem _            = reportUndef "two arguments required"

biDivMod :: [SExpr] -> Lisp SExpr
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

