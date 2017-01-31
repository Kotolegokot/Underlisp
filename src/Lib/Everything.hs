module Lib.Everything where

import Lib.Boolean as Bool
import Lib.Char as Char
import Lib.Control as Ctrl
import Lib.Environment as Env
import Lib.IO as IO
import Lib.List as List
import Lib.Main as Main
import Lib.Math as Math
import Lib.Meta as Meta
import Lib.Ord as Ord
import Lib.Sequence as Sequence
import Lib.Vector as Vector

import Data.IORef
import Base

specialOperators :: [(String, Maybe Int, IORef Scope -> [SExpr] -> EvalM SExpr)]
specialOperators = concat [Bool.specialOperators
                          ,Char.specialOperators
                          ,Ctrl.specialOperators
                          ,Env.specialOperators
                          ,IO.specialOperators
                          ,List.specialOperators
                          ,Main.specialOperators
                          ,Math.specialOperators
                          ,Meta.specialOperators
                          ,Ord.specialOperators
                          ,Sequence.specialOperators
                          ,Vector.specialOperators]
