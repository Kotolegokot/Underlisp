module Lib.Math (builtinSum,
                 builtinSubstract,
                 builtinProduct,
                 builtinDivide,
                 builtinFloat) where
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
                                      NTInt   -> return . int $ fromInt num1 - fromInt num2
                                      NTFloat -> return . float $ fromNumber num1 - fromNumber num2
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
