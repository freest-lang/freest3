-- V exercise 10d

main : Int
main = (\x :Int -> (\y : Int -> x + y)) 10 20
-- result = 30