module Prototype where

data Rest = Rest String | Body String
  deriving (Show, Eq)

-- | arguments, body|rest|nothing, optional|nothing
data Prototype = Prototype { getArgs :: [String]
                           , getOptional :: [String]
                           , getRest :: Maybe Rest }
  deriving Eq

instance Show Prototype where
  show (Prototype args optional rest) = "(" ++ showArgs ++ showOptional ++ showRest ++ ")"
    where showArgs = showList args
          showOptional = if null optional then "" else " &optional " ++ showList optional
          showRest = case rest of
            Just (Rest x) -> " &rest " ++ x
            Just (Body x) -> " &body " ++ x
            Nothing        -> ""

          showList [x]    = x
          showList []     = ""
          showList (x:xs) = x ++ " " ++ showList xs
