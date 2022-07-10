-- V exercise 10i

main : Int
main = (\f : (Int -> Int) -> (\x : Int -> f (f x))) (\y : Int -> y + 1) 10
-- result = 12
