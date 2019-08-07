-- V exercise 10h

applier : (Int -> Int) -> Int -> Int
applier f x = f (f x)

plus1 : Int -> Int
plus1 x = x + 1

main : Int
main = applier plus1 10
-- result = 12