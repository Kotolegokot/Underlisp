module Lib.Math (builtinFunctions
                ,specialOperators) where

import Numeric.Special.Trigonometric
import Base
import Exception

biSum :: [SExpr] -> IO SExpr
biSum sexprs = case numArgs sexprs of
                      NTInt   -> return . int . sum . fmap fromInt $ sexprs
                      NTFloat -> return . float . sum . fmap fromNumber $ sexprs

biSubstract :: [SExpr] -> IO SExpr
biSubstract sexprs@[num1, num2] = case numArgs sexprs of
                                         NTInt   -> return . int $ fromInt num1 - fromInt num2
                                         NTFloat -> return . float $ fromNumber num1 - fromNumber num2
biSubstract _                   = reportUndef "two arguments required"

biProduct :: [SExpr] -> IO SExpr
biProduct sexprs = case numArgs sexprs of
                               NTInt   -> return . int . product . fmap fromInt $ sexprs
                               NTFloat -> return . float . product . fmap fromNumber $ sexprs

biDivide :: [SExpr] -> IO SExpr
biDivide sexprs@[num1, num2] = case numArgs sexprs of
                                      NTInt   -> return . int $ fromInt num1 `div` fromInt num2
                                      NTFloat -> return . float $ fromNumber num1 / fromNumber num2
biDivide _                   = reportUndef "two arguments required"

data NumType = NTInt | NTFloat
numArgs :: [SExpr] -> NumType
numArgs sexprs = numArgs' sexprs NTInt
  where numArgs' (x:xs) NTInt
          | isInt x   = numArgs' xs NTInt
          | isFloat x = numArgs' xs NTFloat
          | otherwise  = report (point x) "float or int expected"
        numArgs' (x:xs) NTFloat
          | isNumber x = numArgs' xs NTFloat
          | otherwise   = report (point x) "float or int expected"
        numArgs' [] return_type = return_type

biFloat :: [SExpr] -> IO SExpr
biFloat [SAtom _ (AInt i)]         = return . float . fromIntegral $ i
biFloat [f@(SAtom _ (AFloat _))]   = return f
biFloat [sexpr]                    = report (point sexpr) "float or int expected"
biFloat _                          = reportUndef "just one argument requried"

biExp :: [SExpr] -> IO SExpr
biExp [num]
  | isNumber num = return . float . exp $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biExp _ = reportUndef "just one argument required"

biLn :: [SExpr] -> IO SExpr
biLn [num]
  | isNumber num = return . float . log $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biLn _ = reportUndef "just one argument required"

biPower :: [SExpr] -> IO SExpr
biPower nums@[num1, num2] = case numArgs nums of
  NTInt   -> return . int $ fromInt num1 ^ fromInt num2
  NTFloat -> return . float $ fromNumber num1 ** fromNumber num2
biPower _ = reportUndef "two arguments required"

biSin :: [SExpr] -> IO SExpr
biSin [num]
  | isNumber num = return . float . sin $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biSin _ = reportUndef "just one argument required"

biCos :: [SExpr] -> IO SExpr
biCos [num]
  | isNumber num = return . float . cos $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biCos _ = reportUndef "just one argument required"

biASin :: [SExpr] -> IO SExpr
biASin [num]
  | isNumber num = return . float . asin $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biASin _ = reportUndef "just one argument required"

biACos :: [SExpr] -> IO SExpr
biACos [num]
  | isNumber num = return . float . acos $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biACos _ = reportUndef "just one argument required"

biATan :: [SExpr] -> IO SExpr
biATan [num]
  | isNumber num = return . float . atan $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biATan _ = reportUndef "just one argument required"

biACot :: [SExpr] -> IO SExpr
biACot [num]
  | isNumber num = return . float . acot $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biACot _ = reportUndef "just one argument required"

biSinH :: [SExpr] -> IO SExpr
biSinH [num]
  | isNumber num = return . float . sinh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biSinH _ = reportUndef "just one argument required"

biCosH :: [SExpr] -> IO SExpr
biCosH [num]
  | isNumber num = return . float . cosh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biCosH _ = reportUndef "just one argument required"

biASinH :: [SExpr] -> IO SExpr
biASinH [num]
  | isNumber num = return . float . asinh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biASinH _ = reportUndef "just one argument required"

biACosH :: [SExpr] -> IO SExpr
biACosH [num]
  | isNumber num = return . float . acosh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biACosH _ = reportUndef "just one argument required"

biATanH :: [SExpr] -> IO SExpr
biATanH [num]
  | isNumber num = return . float . atanh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biATanH _ = reportUndef "just one argument required"

biACotH :: [SExpr] -> IO SExpr
biACotH [num]
  | isNumber num = return . float . acoth $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biACotH _ = reportUndef "just one argument required"

biTruncate :: [SExpr] -> IO SExpr
biTruncate [num]
  | isNumber num = return . int . truncate $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biTruncate _ = reportUndef "just one argument required"

biRound :: [SExpr] -> IO SExpr
biRound [num]
  | isNumber num = return . int . round $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biRound _ = reportUndef "just one argument required"

biCeiling :: [SExpr] -> IO SExpr
biCeiling [num]
  | isNumber num = return . int . ceiling $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biCeiling _ = reportUndef "just one argument required"

biFloor :: [SExpr] -> IO SExpr
biFloor [num]
  | isNumber num = return . int . floor $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
biFloor _ = reportUndef "just one argument required"

biIsNan :: [SExpr] -> IO SExpr
biIsNan [num]
  | isFloat num = return . bool . isNaN $ fromFloat num
  | otherwise   = report (point num) "float or int expected"
biIsNan _ = reportUndef "just one argument required"

biIsInfinite :: [SExpr] -> IO SExpr
biIsInfinite [num]
  | isFloat num = return . bool . isInfinite $ fromFloat num
  | otherwise   = report (point num) "float expected"
biIsInfinite _ = reportUndef "just one argument required"

biIsDenormalized :: [SExpr] -> IO SExpr
biIsDenormalized [num]
  | isFloat num = return . bool . isDenormalized $ fromFloat num
  | otherwise   = report (point num) "float expected"
biIsDenormalized _ = reportUndef "just one argument required"

biIsNegativeZero :: [SExpr] -> IO SExpr
biIsNegativeZero [num]
  | isFloat num = return . bool . isNegativeZero $ fromFloat num
  | otherwise   = report (point num) "float expected"
biIsNegativeZero _ = reportUndef "just one argument required"

biIsIEEE :: [SExpr] -> IO SExpr
biIsIEEE [num]
  | isFloat num = return . bool . isIEEE $ fromFloat num
  | otherwise   = report (point num) "float expected"
biIsIEEE _ = reportUndef "just one argument required"

biQuotRem :: [SExpr] -> IO SExpr
biQuotRem [num1, num2]
  | not $ isInt num1 = report (point num1) "int expected"
  | not $ isInt num2 = report (point num2) "int expected"
  | otherwise        = return $ list [int quot, int rem]
    where (quot, rem) = quotRem (fromInt num1) (fromInt num2)
biQuotRem _ = reportUndef "two arguments required"

biDivMod :: [SExpr] -> IO SExpr
biDivMod [num1, num2]
  | not $ isInt num1 = report (point num1) "int expected"
  | not $ isInt num2 = report (point num2) "int expected"
  | otherwise        = return $ list [int div, int mod]
    where (div, mod) = divMod (fromInt num1) (fromInt num2)
biDivMod _ = reportUndef "two arguments required"

builtinFunctions = [("+",              Nothing,          biSum)
                   ,("-",              Just (2 :: Int),  biSubstract)
                   ,("*",              Nothing,          biProduct)
                   ,("/",              Just 2,           biDivide)
                   ,("float",          Just 1,           biFloat)
                   ,("exp",            Just 1,           biExp)
                   ,("ln",             Just 1,           biLn)
                   ,("^",              Just 2,           biPower)
                   ,("sin",            Just 1,           biSin)
                   ,("cos",            Just 1,           biCos)
                   ,("asin",           Just 1,           biASin)
                   ,("acos",           Just 1,           biACos)
                   ,("atan",           Just 1,           biATan)
                   ,("acot",           Just 1,           biACot)
                   ,("sinh",           Just 1,           biSinH)
                   ,("cosh",           Just 1,           biCosH)
                   ,("truncate",       Just 1,           biTruncate)
                   ,("round",          Just 1,           biRound)
                   ,("ceiling",        Just 1,           biCeiling)
                   ,("floor",          Just 1,           biFloor)
                   ,("nan?",           Just 1,           biIsNan)
                   ,("infinite?",      Just 1,           biIsInfinite)
                   ,("denormalized?",  Just 1,           biIsDenormalized)
                   ,("negative-zero?", Just 1,           biIsNegativeZero)
                   ,("IEEE?",          Just 1,           biIsIEEE)
                   ,("quot-rem",       Just 1,           biQuotRem)
                   ,("div-mod",        Just 1,           biDivMod)]

specialOperators = []

