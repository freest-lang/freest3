-- V exercise 10f

main : Int
main = (\x : Int -> (\y : Int -> x + y)) 3 7
-- result = 10
