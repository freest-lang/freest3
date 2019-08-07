-- V exercise 10e

add : Int -> Int -> Int
add x y = x + y

addTo7 : (Int -> Int)
addTo7 = add 7

main : Int
main = addTo7 10
-- result = 17