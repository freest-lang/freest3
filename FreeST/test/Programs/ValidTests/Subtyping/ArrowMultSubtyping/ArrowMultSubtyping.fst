f  : (Int 1-> Int) -> Int 1-> Int 
f g n = g n 

main : Int
main = f (\n:Int -> n + 1) 0