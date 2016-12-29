module LispShow where

import Debug.Trace (trace)

class LispShow a where
  lispShow :: a -> String

instance LispShow a => LispShow [a] where
  lispShow xs = "(" ++ lispShow' xs ++ ")"
    where lispShow' [x]    = lispShow x
          lispShow' (x:xs) = lispShow x ++ " " ++ lispShow' xs
          lispShow' []     = ""

lispPrint :: LispShow a => a -> IO ()
lispPrint = putStrLn . lispShow

lispTrace :: LispShow a => a -> b -> b
lispTrace = trace . lispShow

lispTraceId :: LispShow a => a -> a
lispTraceId a = lispTrace a a
