-- V exercise 10e

addTo7 : (Int -> Int)
addTo7 = (\x : Int -> (\y : Int -> x + y)) 7

main : Int
main = addTo7 10
-- result = 17
