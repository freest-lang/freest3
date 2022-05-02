module CycleB.CycleB where

-- Invalid because we commented this line, so that g is out of scope

-- import Cycle

f : Int
f = id[Int] $ 2 + g
