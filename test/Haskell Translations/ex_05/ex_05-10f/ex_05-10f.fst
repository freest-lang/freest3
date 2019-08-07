-- V exercise 10f

add : Int -> Int -> Int
add x y = x + y

add7 : (Int -> Int)
add7 = add 7

add3To7 : Int
add3To7 = add7 3

main : Int
main = add3To7
-- result = 10