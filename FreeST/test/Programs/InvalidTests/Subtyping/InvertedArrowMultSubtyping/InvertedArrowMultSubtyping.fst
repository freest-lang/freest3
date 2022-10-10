f  : (Int -> Int) -> Int -> Int 
f g n = g $ g n 

main : Int
main = f (\n:Int 1-> n + 1) 0