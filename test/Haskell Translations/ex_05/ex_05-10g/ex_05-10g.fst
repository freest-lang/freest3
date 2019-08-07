-- V exercise 10g

add : Int -> (Int -> Int)
add x = add' x

add' : Int -> Int -> Int
add' x y = x + y

main : Int 
main = add 10 20
-- result = 30