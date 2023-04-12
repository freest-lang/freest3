f : Int 1-> Int
f x = 2 * x

g : (Int 1-> Int) -> Int
g h = h (h 5)

main : Int
main = g f
