module Lib.Math (builtin_sum,
                 builtin_substract,
                 builtin_product,
                 builtin_divide,
                 builtin_float) where
            
import SExpr
import Lib.Internal

builtin_sum :: [SExpr] -> IO SExpr
builtin_sum sexprs = case num_args sexprs of
                       NTInt   -> return . SInt . sum . fmap from_int $ sexprs
                       NTFloat -> return . SFloat . sum . fmap from_number $ sexprs

builtin_substract :: [SExpr] -> IO SExpr
builtin_substract sexprs@[num1, num2] = case num_args sexprs of
                                          NTInt   -> return . SInt $ from_int num1 - from_int num2
                                          NTFloat -> return . SFloat $ from_number num1 - from_number num2
builtin_substract _                   = error "-: two arguments required"

builtin_product :: [SExpr] -> IO SExpr
builtin_product sexprs = case num_args sexprs of
                                NTInt   -> return . SInt . product . fmap from_int $ sexprs
                                NTFloat -> return . SFloat . product . fmap from_number $ sexprs

builtin_divide :: [SExpr] -> IO SExpr
builtin_divide sexprs@[num1, num2] = case num_args sexprs of
                                       NTInt   -> return . SInt $ from_int num1 - from_int num2
                                       NTFloat -> return . SFloat $ from_number num1 - from_number num2
builtin_divide _                   = error "/: two arguments required"

data NumType = NTInt | NTFloat
num_args :: [SExpr] -> NumType
num_args sexprs = num_args' sexprs NTInt
  where num_args' (x:xs) NTInt = case x of
                                   SInt _   -> num_args' xs NTInt
                                   SFloat _ -> num_args' xs NTFloat
                                   _        -> error "float or int expected"
        num_args' (x:xs) NTFloat = if is_number x
                                      then num_args' xs NTFloat
                                      else error "float or int expected"
        num_args' [] return_type = return_type

builtin_float :: [SExpr] -> IO SExpr
builtin_float [SInt int]         = return . SFloat . fromIntegral $ int
builtin_float [float@(SFloat _)] = return float
builtin_float [_]                = error "float: float or int expected"
builtin_float _                  = error "float: just one argument requried"
