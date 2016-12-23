module LispShow where

import Debug.Trace (trace)

class LispShow a where
  lisp_show :: a -> String

instance LispShow a => LispShow [a] where
  lisp_show xs = "(" ++ lisp_show' xs ++ ")"
    where lisp_show' [x]    = lisp_show x
          lisp_show' (x:xs) = lisp_show x ++ " " ++ lisp_show xs
          lisp_show' []     = ""

lisp_print :: LispShow a => a -> IO ()
lisp_print = putStrLn . lisp_show

lisp_trace :: LispShow a => a -> b -> b
lisp_trace = trace . lisp_show

lisp_trace_id :: LispShow a => a -> a
lisp_trace_id a = lisp_trace a a
