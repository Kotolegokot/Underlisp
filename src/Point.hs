module Point where

data Point = Point { pFilename :: String
                   , pRow      :: Int
                   , pColumn   :: Int }
           | Undefined
  deriving (Eq, Ord, Show, Read)

row :: (Int -> Int) -> Point -> Point
row _ Undefined = Undefined
row f point     = point { pRow = f $ pRow point }

column :: (Int -> Int) -> Point -> Point
column _ Undefined = Undefined
column f point     = point { pColumn = f $ pColumn point }

forwardRow :: Point -> Point
forwardRow = row (+1) . column (const 0)

forwardColumn :: Point -> Point
forwardColumn = column (+1)

forward :: Char -> Point -> Point
forward '\n' = forwardRow
forward _    = forwardColumn

startPoint :: String -> Point
startPoint filename = Point filename 1 0
