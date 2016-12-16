module Util (shit
            , Point (..)
            , forward_row
            , forward_column
            , start_point) where

shit :: Point -> String -> a
shit Undefined                   msg = error msg
shit (Point filename row column) msg = error $ prefix ++ msg
  where prefix = filename ++ ":" ++ show column ++ ":" ++ show row ++ ": error: "
data Point = Point { filename :: String
                   , row :: Int
                   , column :: Int }
           | Undefined
  deriving (Eq, Show)

forward_row :: Point -> Point
forward_row Undefined = Undefined
forward_row point = point { row = row point + 1, column = 0 }

forward_column :: Point -> Point
forward_column Undefined = Undefined
forward_column point = point { column = column point + 1 }

start_point :: String -> Point
start_point filename = Point filename 0 0
