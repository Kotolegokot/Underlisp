module Prototype where

import LispShow

data Prototype = Prototype [String] Bool
  deriving (Eq, Show)

instance LispShow Prototype where
  lispShow (Prototype args rest)
    | rest      = "(" ++ showList args ++ ")"
    | otherwise = "(" ++ showList args ++ ")"
    where showList [x]    = if rest then x else "&rest " ++ x
          showList (x:xs) = x ++ " " ++ showList xs
          showList []     = []
