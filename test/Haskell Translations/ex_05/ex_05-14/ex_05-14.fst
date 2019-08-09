-- V exercise 14

main : Int
main = (\f : (Int->Int->Int) -> (\x : Int -> (\y : Int -> f y x))) (-) 10 20
--result = 10