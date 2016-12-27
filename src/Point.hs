module Point where

data Point = Point { _filename :: String
                   , _row      :: Int
                   , _column   :: Int }
           | Undefined
  deriving (Eq, Ord, Show)

row :: (Int -> Int) -> Point -> Point
row _ Undefined = Undefined
row f point     = point { _row = f $ _row point }

column :: (Int -> Int) -> Point -> Point
column _ Undefined = Undefined
column f point     = point { _column = f $ _column point }

forward_row :: Point -> Point
forward_row = row (+1)

forward_column :: Point -> Point
forward_column = column (+1)

forward :: Char -> Point -> Point
forward '\n' = forward_row
forward _    = forward_column

start_point :: String -> Point
start_point filename = Point filename 1 0

report :: Point -> String -> a
report Undefined                   msg = error msg
report (Point filename row column) msg = error $ prefix ++ msg
  where prefix = filename ++ ":" ++ show row ++ ":" ++ show column ++ ": error: "
