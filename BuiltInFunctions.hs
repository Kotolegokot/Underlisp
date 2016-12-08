module BuiltInFunctions
    (print_,
     print_ln_,
     type_,
     add_,
     substract_,
     product_,
     divide_,
     equal_,
     inequal_,
     lt_,
     gt_,
     le_,
     ge_,
     and_,
     or_,
     impl_,
     not_,
     float_
    ) where

import SemanticAnalyzer

type Func = [Terminal] -> IO Terminal

print_ :: Func
print_ args
  | length args /= 1 = error "'print' requires only one argument"
  | otherwise        = (putStr . printTerminal) (head args) >> return TNil

print_ln_ :: Func
print_ln_ args
  | length args /= 1 = error "'print-ln' requires only one argument"
  | otherwise        = (putStrLn . printTerminal) (head args) >> return TNil

type_ :: Func
type_ args
  | length args /= 1 = error "'type' requires only one argument"
  | otherwise        = return . TString . printType $ head args

add_ :: Func
add_ args = return $ case check_num_args args of
                       ARFloat -> TFloat . sum $ fmap fromNumber args
                       ARInt   -> TInt . sum $ fmap fromInt args

substract_ :: Func
substract_ args@[x, y] = return $ case check_num_args args of
                                      ARFloat -> TFloat $ fromNumber x - fromNumber y
                                      ARInt -> TInt $ fromInt x - fromInt y

substract_ _             = error "'-' requires two arguments"

product_ :: Func
product_ args = return $ case check_num_args args of
                           ARFloat -> TFloat . product $ fmap fromNumber args
                           ARInt -> TInt . product $ fmap fromInt args

divide_ :: Func
divide_ args@[x, y] = return $ case check_num_args args of
                                   ARFloat -> TFloat $ fromNumber x / fromNumber y
                                   ARInt -> TInt $ fromInt x `div` fromInt y

divide_ _             = error "'/' requires two arguments"

equal_ :: Func
equal_ (x:xs) = return . boolToTerminal . and $ fmap (x==) xs

inequal_ :: Func
ineuqal_ [x, y] = return . boolToTerminal $ x /= y
inequal_ _      = error "'/=' requires two arguments"

lt_ :: Func
lt_ [x, y] = return . boolToTerminal $ x < y
lt_ _      = error "'<' requires two arguments"

gt_ :: Func
gt_ [x, y] = return . boolToTerminal $ x > y
gt_ _      = error "'>' requires two arguments"

le_ :: Func
le_ [x, y] = return . boolToTerminal $ x <= y
le_ _      = error "'<=' requires two arguments"

ge_ :: Func
ge_ [x, y] = return . boolToTerminal $ x >= y
ge_ _      = error "'>=' requires two arguments"

and_ :: Func
and_ args = return . boolToTerminal . and . fmap terminalToBool $ args

or_ :: Func
or_ args = return . boolToTerminal . or . fmap terminalToBool $ args

impl_ :: Func
impl_ [x, y] = return . boolToTerminal $ (not $ terminalToBool x) || terminalToBool y
impl_ _      = error "'->' requires two arguments"

not_ :: Func
not_ [x] = return . boolToTerminal . not . terminalToBool $ x
not_ _   = error "'not' requires only one argument"

float_ :: Func
float_ [x] = return $ case x of
                        TFloat float -> TFloat float
                        TInt int     -> TFloat . fromIntegral $ int
                        _            -> error "float or int expected"

float_ _   = error "'float' requires only one argument"

data ArithmReturn = ARInt | ARFloat
check_num_args :: [Terminal] -> ArithmReturn
check_num_args args = helper args ARInt
    where helper (x:xs) ARInt = case x of
                                  TInt _   -> helper xs ARInt
                                  TFloat _ -> helper xs ARFloat
                                  _        -> error "float or int expected"

          helper (x:xs) ARFloat = case x of
                                   TInt _   -> helper xs ARFloat
                                   TFloat _ -> helper xs ARFloat
                                   _        -> error "float or int expected"

          helper [] return_type = return_type
