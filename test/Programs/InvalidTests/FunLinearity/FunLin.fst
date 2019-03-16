g : (Int -o Int) -> Int
g f = f (f 5)

f : Int -o Int
f x = 2 * x

main : Int
main = g 
