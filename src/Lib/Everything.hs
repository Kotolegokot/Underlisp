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

import Base

specialOperators :: [(String, Maybe Int, Env -> [SExpr] -> Eval (Env, SExpr))]
specialOperators = concat [Bool.specialOperators
                          ,Char.specialOperators
                          ,Ctrl.specialOperators
                          ,Env.specialOperators
                          ,IO.specialOperators
                          ,List.specialOperators
                          ,Main.specialOperators
                          ,Math.specialOperators
                          ,Meta.specialOperators
                          ,Ord.specialOperators]

builtinFunctions :: [(String, Maybe Int, [SExpr] -> Eval SExpr)]
builtinFunctions = concat [Bool.builtinFunctions
                          ,Char.builtinFunctions
                          ,Ctrl.builtinFunctions
                          ,Env.builtinFunctions
                          ,IO.builtinFunctions
                          ,List.builtinFunctions
                          ,Main.builtinFunctions
                          ,Math.builtinFunctions
                          ,Meta.builtinFunctions
                          ,Ord.builtinFunctions]
