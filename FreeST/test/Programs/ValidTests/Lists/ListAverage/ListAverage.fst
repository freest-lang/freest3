import List

data List = Nil | Cons Float List

main : Float
main = let list = Cons 2.2 (Cons 3.7 (Cons 7.2 (Nil))) in
        average list

average : List -> Float
average l = sum' l /. fromInteger (length' l)

sum' : List -> Float
sum' Nil = 0.0
sum' (Cons x xs) = x +. sum' xs

length' : List -> Int
length' Nil = 0
length' (Cons x xs) = 1 + length' xs

