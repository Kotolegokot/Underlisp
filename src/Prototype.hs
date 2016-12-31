module Prototype where

data Prototype = Prototype [String] Bool
  deriving Eq

instance Show Prototype where
  show (Prototype args rest) = "(" ++ showList args ++ ")"
    where showList [x]    = if rest then "&rest " ++ x else x
          showList (x:xs) = x ++ " " ++ showList xs
          showList []     = []
