module ModuleB where

import LongCycle


double : Int -> Int
double = \x:Int -> 2 * x


g : Int -> Int
g x = 3 * h x
