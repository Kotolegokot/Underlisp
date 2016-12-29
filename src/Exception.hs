module Exception (LispError (..)
                 , report
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
report point msg = throw $ LispError point "" msg

reportUndef :: String -> a
reportUndef = report Undefined

rethrow :: Exception e => (e -> e) -> IO a -> IO a
rethrow f = handle (\e -> throw $ f e)
