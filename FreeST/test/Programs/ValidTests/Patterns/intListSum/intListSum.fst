data IntList = Nil | Cons Int IntList

sum' : IntList -> Int
sum' Nil        = 0
sum' (Cons x y) = x + sum' y

main : Int
main = sum' (Cons 5 (Cons 7 (Cons 2 (Cons 6 (Cons 3 Nil)))))

