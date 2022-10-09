data T = A | B | C

f : T -> Int
f A = 0
f B = 1
f x = 2

main : Int
main = f A + f C