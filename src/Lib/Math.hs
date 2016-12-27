module Lib.Math (builtin_sum,
                 builtin_substract,
                 builtin_product,
                 builtin_divide,
                 builtin_float) where
import SExpr

builtin_sum :: [SExpr] -> IO SExpr
builtin_sum sexprs = case num_args sexprs of
                       NTInt   -> return . int . sum . fmap from_int $ sexprs
                       NTFloat -> return . float . sum . fmap from_number $ sexprs

builtin_substract :: [SExpr] -> IO SExpr
builtin_substract sexprs@[num1, num2] = case num_args sexprs of
                                          NTInt   -> return . int $ from_int num1 - from_int num2
                                          NTFloat -> return . float $ from_number num1 - from_number num2
builtin_substract _                   = error "-: two arguments required"

builtin_product :: [SExpr] -> IO SExpr
builtin_product sexprs = case num_args sexprs of
                                NTInt   -> return . int . product . fmap from_int $ sexprs
                                NTFloat -> return . float . product . fmap from_number $ sexprs

builtin_divide :: [SExpr] -> IO SExpr
builtin_divide sexprs@[num1, num2] = case num_args sexprs of
                                       NTInt   -> return . int $ from_int num1 - from_int num2
                                       NTFloat -> return . float $ from_number num1 - from_number num2
builtin_divide _                   = error "/: two arguments required"

data NumType = NTInt | NTFloat
num_args :: [SExpr] -> NumType
num_args sexprs = num_args' sexprs NTInt
  where num_args' (x:xs) NTInt
          | is_int x   = num_args' xs NTInt
          | is_float x = num_args' xs NTFloat
          | otherwise  = error "float or int expected"
        num_args' (x:xs) NTFloat
          | is_number x = num_args' xs NTFloat
          | otherwise   = error "float or int expected"
        num_args' [] return_type = return_type

builtin_float :: [SExpr] -> IO SExpr
builtin_float [SAtom _ (AInt i)]         = return . float . fromIntegral $ i
builtin_float [f@(SAtom _ (AFloat _))]   = return f
builtin_float [sexpr]                    = report (point sexpr) "float: float or int expected"
builtin_float _                          = error "float: just one argument requried"
