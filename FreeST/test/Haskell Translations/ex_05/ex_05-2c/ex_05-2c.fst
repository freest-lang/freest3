-- V exercise 2c

add3 : Int -> (Int -> Int)
add3 x = sum2 x

sum2 : Int -> Int -> Int
sum2 x y = x + y

main : Int
main = (add3 10) 20
-- result = 30
