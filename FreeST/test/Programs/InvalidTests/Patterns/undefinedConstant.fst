-- This is invalid since f 2 is not defined.

f : Int -> Int -> Int
f 1 = (+1)

main : Int
main = f 2 2