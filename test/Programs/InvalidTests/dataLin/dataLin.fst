data T = C Int -o Int -- Invalid

f : Int -o Int  -- Invalid
f x = x

main : Int
main = case C f of { g -> g (g 5) } 
