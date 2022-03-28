type InfinitePair = rec a . (a, Int)

f : Int -> InfinitePair
f x = (f x, x + 1)

main : Int
main = 5
