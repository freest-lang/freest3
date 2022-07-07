

data List = Nil | Cons Int List

f : List -> Int -> List -> Int
f Nil         0 Nil         = 0
f (Cons x xs) 1 (Cons y ys) = 1
f (Cons x xs) z (Cons y ys) = x+z+y

main : Int
main = f Nil 1 Nil