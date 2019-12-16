type InfinitePair = rec x . (x, Int)

f : Int -> rec x . (x, Int)
f x = (f x, x + 1)

main : Int
main = 5
