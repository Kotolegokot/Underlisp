module Exception (LispError (..)
                 , report
                 , report_undef
                 , catch
                 , throw
                 , handle) where

import System.IO
import Data.Typeable
import Data.Dynamic
import Control.Exception

import Point

data LispError = LispError { le_point :: Point
                           , le_cmd   :: String
                           , le_msg   :: String }
  deriving (Eq, Typeable)

instance Exception LispError
instance Show LispError where
  show (LispError Undefined cmd msg)                   = cmd ++ ": " ++ msg
  show (LispError (Point filename row column) cmd msg) = msg'
    where msg' = filename ++ ":" ++ show row ++ ":" ++ show column ++  ": " ++ cmd ++ ": " ++ msg

report :: Point -> String -> a
report point msg = throw $ LispError point "" msg

report_undef :: String -> a
report_undef = report Undefined
