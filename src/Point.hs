module Point where

data Point = Point { p_filename :: String
                   , p_row      :: Int
                   , p_column   :: Int }
           | Undefined
  deriving (Eq, Ord, Show, Read)

row :: (Int -> Int) -> Point -> Point
row _ Undefined = Undefined
row f point     = point { p_row = f $ p_row point }

column :: (Int -> Int) -> Point -> Point
column _ Undefined = Undefined
column f point     = point { p_column = f $ p_column point }

forward_row :: Point -> Point
forward_row = row (+1) . column (const 0)

forward_column :: Point -> Point
forward_column = column (+1)

forward :: Char -> Point -> Point
forward '\n' = forward_row
forward _    = forward_column

start_point :: String -> Point
start_point filename = Point filename 1 0
