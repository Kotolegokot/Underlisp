module Point where

data Point = Point { filename :: String
                   , row      :: Int
                   , column   :: Int }
           | Undefined
  deriving (Eq, Show)

forward_row :: Point -> Point
forward_row Undefined = Undefined
forward_row point = point { row = row point + 1, column = 0 }

forward_column :: Point -> Point
forward_column Undefined = Undefined
forward_column point = point { column = column point + 1 }

forward :: Char -> Point -> Point
forward '\n' = forward_row
forward _    = forward_column

start_point :: String -> Point
start_point filename = Point filename 0 0
