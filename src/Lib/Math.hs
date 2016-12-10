module Lib.Math (builtin_sum,
                 builtin_substract,
                 builtin_product,
                 builtin_divide,
                 builtin_float) where
            
import Expr
import Lib.Internal

builtin_sum :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_sum eval context args = do
    (exps, return_type) <- num_args eval context args
    return (case return_type of
              NTInt   -> SInt . sum . fmap from_int $ exps
              NTFloat -> SFloat . sum . fmap from_number $ exps,
            context)

builtin_substract :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_substract eval context args@[_, _] = do
    ([x, y], return_type) <- num_args eval context args
    return (case return_type of
              NTInt   -> SInt   $ from_int x - from_int y
              NTFloat -> SFloat $ from_number x - from_number y,
            context)
builtin_substract _    _       _           = error "'-' expects requires two arguments"

builtin_product :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_product eval context args = do
    (exps, return_type) <- num_args eval context args
    return (case return_type of
              NTInt   -> SInt . product . fmap from_int $ exps
              NTFloat -> SFloat . product . fmap from_number $ exps,
            context)

builtin_divide :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_divide eval context args@[_, _] = do
    ([x, y], return_type) <- num_args eval context args
    return (case return_type of
              NTInt   -> SInt   $ from_int x `div` from_int y
              NTFloat -> SFloat $ from_number x / from_number y,
            context)
builtin_divide _    _       _           = error "'-' expects requires two arguments"

data NumType = NTInt | NTFloat
num_args :: Eval -> Context -> [SExpr] -> IO ([SExpr], NumType)
num_args eval context args = helper args [] NTInt
    where helper (x:xs) acc NTInt = do
            (expr, _) <- eval context x
            case expr of
              SInt   _ -> helper xs (expr : acc) NTInt
              SFloat _ -> helper xs (expr : acc) NTFloat
              _        -> error "float or int expected"

          helper (x:xs) acc NTFloat = do
              (expr, _) <- eval context x
              case expr of
                SInt   _ -> helper xs (expr : acc) NTFloat
                SFloat _ -> helper xs (expr : acc) NTFloat
                _        -> error "float or int expected"

          helper [] acc return_type = return (reverse acc, return_type)

builtin_float :: Eval -> Context -> [SExpr] -> IO (SExpr, Context)
builtin_float eval context [arg] = do
    (expr, _) <- eval context arg
    return (case expr of
              SFloat float -> SFloat float
              SInt   int   -> SFloat $ fromIntegral int
              _            -> error "float or int expected",
            context)
builtin_float _    _       _     = error "'float' requires just one argument"
