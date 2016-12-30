module Lib.Math (builtinSum
                , builtinSubstract
                , builtinProduct
                , builtinDivide
                , builtinFloat
                , builtinExp
                , builtinLn
                , builtinSqrt
                , builtinPower
                , builtinLog
                , builtinSin
                , builtinCos
                , builtinTan
                , builtinCot
                , builtinASin
                , builtinACos
                , builtinATan
                , builtinACot
                , builtinSinH
                , builtinCosH
                , builtinTanH
                , builtinCotH
                , builtinASinH
                , builtinACosH
                , builtinATanH
                , builtinACotH
                , builtinTruncate
                , builtinRound
                , builtinCeiling
                , builtinFloor
                , builtinIsNan
                , builtinIsInfinite
                , builtinIsDenormalized
                , builtinIsNegativeZero
                , builtinIsIEEE) where

import Numeric.Special.Trigonometric
import SExpr
import Exception

builtinSum :: [SExpr] -> IO SExpr
builtinSum sexprs = case numArgs sexprs of
                      NTInt   -> return . int . sum . fmap fromInt $ sexprs
                      NTFloat -> return . float . sum . fmap fromNumber $ sexprs

builtinSubstract :: [SExpr] -> IO SExpr
builtinSubstract sexprs@[num1, num2] = case numArgs sexprs of
                                         NTInt   -> return . int $ fromInt num1 - fromInt num2
                                         NTFloat -> return . float $ fromNumber num1 - fromNumber num2
builtinSubstract _                   = reportUndef "two arguments required"

builtinProduct :: [SExpr] -> IO SExpr
builtinProduct sexprs = case numArgs sexprs of
                               NTInt   -> return . int . product . fmap fromInt $ sexprs
                               NTFloat -> return . float . product . fmap fromNumber $ sexprs

builtinDivide :: [SExpr] -> IO SExpr
builtinDivide sexprs@[num1, num2] = case numArgs sexprs of
                                      NTInt   -> return . int $ fromInt num1 `div` fromInt num2
                                      NTFloat -> return . float $ fromNumber num1 / fromNumber num2
builtinDivide _                   = reportUndef "two arguments required"

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

builtinFloat :: [SExpr] -> IO SExpr
builtinFloat [SAtom _ (AInt i)]         = return . float . fromIntegral $ i
builtinFloat [f@(SAtom _ (AFloat _))]   = return f
builtinFloat [sexpr]                    = report (point sexpr) "float or int expected"
builtinFloat _                          = reportUndef "just one argument requried"

builtinExp :: [SExpr] -> IO SExpr
builtinExp [num]
  | isNumber num = return . float . exp $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinExp _ = reportUndef "just one argument required"

builtinLn :: [SExpr] -> IO SExpr
builtinLn [num]
  | isNumber num = return . float . log $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinLn _ = reportUndef "just one argument required"

builtinSqrt :: [SExpr] -> IO SExpr
builtinSqrt [num]
  | isNumber num = return . float . sqrt $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinSqrt _ = reportUndef "just one argument required"

builtinPower :: [SExpr] -> IO SExpr
builtinPower nums@[num1, num2] = case numArgs nums of
  NTInt   -> return . int $ fromInt num1 ^ fromInt num2
  NTFloat -> return . float $ fromNumber num1 ** fromNumber num2
builtinPower _ = reportUndef "two arguments required"

builtinLog :: [SExpr] -> IO SExpr
builtinLog nums@[num1, num2]
  | not $ isNumber num1 = report (point num1) "float or int expected"
  | not $ isNumber num2 = report (point num2) "float or int expected"
  | otherwise           = return . float $ logBase (fromNumber num1) (fromNumber num2)
builtinLog _ = reportUndef "two arguments required"

builtinSin :: [SExpr] -> IO SExpr
builtinSin [num]
  | isNumber num = return . float . sin $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinSin _ = reportUndef "just one argument required"

builtinCos :: [SExpr] -> IO SExpr
builtinCos [num]
  | isNumber num = return . float . cos $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinCos _ = reportUndef "just one argument required"

builtinTan :: [SExpr] -> IO SExpr
builtinTan [num]
  | isNumber num = return . float . tan $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinTan _ = reportUndef "just one argument required"

builtinCot :: [SExpr] -> IO SExpr
builtinCot [num]
  | isNumber num = return . float . cot $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinCot _ = reportUndef "just one argument requried"

builtinASin :: [SExpr] -> IO SExpr
builtinASin [num]
  | isNumber num = return . float . asin $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinASin _ = reportUndef "just one argument required"

builtinACos :: [SExpr] -> IO SExpr
builtinACos [num]
  | isNumber num = return . float . acos $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinACos _ = reportUndef "just one argument required"

builtinATan :: [SExpr] -> IO SExpr
builtinATan [num]
  | isNumber num = return . float . atan $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinATan _ = reportUndef "just one argument required"

builtinACot :: [SExpr] -> IO SExpr
builtinACot [num]
  | isNumber num = return . float . acot $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinACot _ = reportUndef "just one argument required"

builtinSinH :: [SExpr] -> IO SExpr
builtinSinH [num]
  | isNumber num = return . float . sinh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinSinH _ = reportUndef "just one argument required"

builtinCosH :: [SExpr] -> IO SExpr
builtinCosH [num]
  | isNumber num = return . float . cosh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinCosH _ = reportUndef "just one argument required"

builtinTanH :: [SExpr] -> IO SExpr
builtinTanH [num]
  | isNumber num = return . float . tanh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinTanH _ = reportUndef "just one argument required"

builtinCotH :: [SExpr] -> IO SExpr
builtinCotH [num]
  | isNumber num = return . float . coth $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinCotH _ = reportUndef "just one argument required"

builtinASinH :: [SExpr] -> IO SExpr
builtinASinH [num]
  | isNumber num = return . float . asinh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinASinH _ = reportUndef "just one argument required"

builtinACosH :: [SExpr] -> IO SExpr
builtinACosH [num]
  | isNumber num = return . float . acosh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinACosH _ = reportUndef "just one argument required"

builtinATanH :: [SExpr] -> IO SExpr
builtinATanH [num]
  | isNumber num = return . float . atanh $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinATanH _ = reportUndef "just one argument required"

builtinACotH :: [SExpr] -> IO SExpr
builtinACotH [num]
  | isNumber num = return . float . acoth $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinACotH _ = reportUndef "just one argument required"

builtinTruncate :: [SExpr] -> IO SExpr
builtinTruncate [num]
  | isNumber num = return . int . truncate $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinTruncate _ = reportUndef "just one argument required"

builtinRound :: [SExpr] -> IO SExpr
builtinRound [num]
  | isNumber num = return . int . round $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinRound _ = reportUndef "just one argument required"

builtinCeiling :: [SExpr] -> IO SExpr
builtinCeiling [num]
  | isNumber num = return . int . ceiling $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinCeiling _ = reportUndef "just one argument required"

builtinFloor :: [SExpr] -> IO SExpr
builtinFloor [num]
  | isNumber num = return . int . floor $ fromNumber num
  | otherwise    = report (point num) "float or int expected"
builtinFloor _ = reportUndef "just one argument required"

builtinIsNan :: [SExpr] -> IO SExpr
builtinIsNan [num]
  | isFloat num = return . bool . isNaN $ fromFloat num
  | otherwise   = report (point num) "float or int expected"
builtinIsNan _ = reportUndef "just one argument required"

builtinIsInfinite :: [SExpr] -> IO SExpr
builtinIsInfinite [num]
  | isFloat num = return . bool . isInfinite $ fromFloat num
  | otherwise   = report (point num) "float expected"
builtinIsInfinite _ = reportUndef "just one argument required"

builtinIsDenormalized :: [SExpr] -> IO SExpr
builtinIsDenormalized [num]
  | isFloat num = return . bool . isDenormalized $ fromFloat num
  | otherwise   = report (point num) "float expected"
builtinIsDenormalized _ = reportUndef "just one argument required"

builtinIsNegativeZero :: [SExpr] -> IO SExpr
builtinIsNegativeZero [num]
  | isFloat num = return . bool . isNegativeZero $ fromFloat num
  | otherwise   = report (point num) "float expected"
builtinIsNegativeZero _ = reportUndef "just one argument required"

builtinIsIEEE :: [SExpr] -> IO SExpr
builtinIsIEEE [num]
  | isFloat num = return . bool . isIEEE $ fromFloat num
  | otherwise   = report (point num) "float expected"
builtinIsIEEE _ = reportUndef "just one argument required"
