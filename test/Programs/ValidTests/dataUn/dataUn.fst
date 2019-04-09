data T = C Int -> Int

f : Int -> Int
f x = x

main : Int
main = case C f of { g -> g (g 5) } 
