data T = C Int -o Int

f : Int -o Int
f x = x

main : Int
main = case C f of { 5 } 
