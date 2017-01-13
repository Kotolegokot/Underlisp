module Exception (LispError (..)
                 , report
                 , reportCmd
                 , reportUndef
                 , catch
                 , throw
                 , rethrow
                 , handle) where

import System.IO
import Data.Typeable
import Data.Dynamic
import Control.Exception

import Point

data LispError = LispError { lePoint :: Point
                           , leCmd   :: String
                           , leMsg   :: String }
  deriving (Eq, Typeable)

instance Exception LispError

instance Show LispError where
  show (LispError Undefined cmd msg)                   = cmd ++ ": " ++ msg
  show (LispError (Point filename row column) cmd msg) = msg'
    where msg' = filename ++ ":" ++ show row ++ ":" ++ show column ++  ": " ++ cmd ++ ": " ++ msg

report :: Point -> String -> a
report point = throw . LispError point ""

reportCmd :: Point -> String -> String -> a
reportCmd = throw .:: LispError
  where (.::) = (.) . (.) . (.)

reportUndef :: String -> a
reportUndef = report Undefined

rethrow :: Exception e => (e -> e) -> IO a -> IO a
rethrow f = handle (\e -> throw $ f e)
