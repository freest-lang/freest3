
data List = Nil | List Int List

f : List -> Int
f Nil = -1
f (List x Nil) = x
f (List x xs)  = x*10

main : Int
main = f $ List 1 $ List 2 $ List 3 Nil