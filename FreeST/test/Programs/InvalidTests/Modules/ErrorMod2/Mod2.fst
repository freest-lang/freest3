module Mod2 where


g : Int -> Int
g z = let (c,s) = new @(!Int;Close) () in z
