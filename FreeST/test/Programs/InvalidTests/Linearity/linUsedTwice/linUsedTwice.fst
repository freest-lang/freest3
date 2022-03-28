f : Int -o Int
f x = 2 * x

g : (Int -o Int) -> Int
g h = h (h 5)

main : Int
main = g f
