f : &{A: Skip, B: Skip} -> Int
f c = match c with {A _ -> 5, B _ -> 6, C _ -> 7}

main : Int
main = 5
