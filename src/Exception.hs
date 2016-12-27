module Exception (LispError (..)
                 , handle_lisp_error
                 , report
                 , catch
                 , throw
                 , handle) where

import System.IO
import Data.Typeable
import Data.Dynamic
import Control.Exception

import Point
import Util

data LispError = LispError { le_point :: Point
                           , le_msg   :: String }
  deriving (Eq, Show, Read, Typeable)

instance Exception LispError

report :: Point -> String -> a
report = throw .: LispError

handle_lisp_error :: IO () -> IO ()
handle_lisp_error = (`catch` show_error)
  where show_error (LispError Undefined msg)                   = hPutStrLn stderr msg
        show_error (LispError (Point filename row column) msg) = hPutStrLn stderr msg'
          where msg' = filename ++ ":" ++ show row ++ ":" ++ show column ++ ": error: " ++ msg
