f : Skip;Skip -> Int
f x = 1


main : Int
main = f (fst @Skip @Skip $ new Skip)
