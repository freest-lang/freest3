

data T = A T | B | C

f : T -> T -> Int
f _ _ = 0

main : Int
main = f B B