-- V exercise 10h

plus1 : Int -> Int
plus1 x = x + 1

main : Int
main = (\f : (Int -> Int) -> (\x : Int -> f (f x))) plus1 10
-- result = 12