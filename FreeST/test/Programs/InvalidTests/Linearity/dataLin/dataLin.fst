data T = C Int 1-> Int

f : Int 1-> Int
f x = x

main : Int
main = case C f of {C g -> g (g 5) } 
