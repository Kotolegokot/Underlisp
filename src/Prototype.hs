module Prototype where

import LispShow

data Prototype = Prototype [String] Bool
  deriving (Eq, Show)

instance LispShow Prototype where
  lisp_show (Prototype args rest)
    | rest      = "(" ++ show_list args ++ ")"
    | otherwise = "(" ++ show_list args ++ ")"
    where show_list [x]    = if rest then x else "&rest " ++ x
          show_list (x:xs) = x ++ " " ++ show_list xs
          show_list []     = []
