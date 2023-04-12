g : (Int 1-> Int) -> Int
g f = f (f 5)

f : Int 1-> Int
f x = 2 * x

main : Int
main = g 
