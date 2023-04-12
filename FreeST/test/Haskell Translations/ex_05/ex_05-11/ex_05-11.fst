-- V exercise 11

main : Int
main = (\x :Int -> (\y :Int -> (\z :Int -> x*y*z))) 1 2 3
-- result = 6
