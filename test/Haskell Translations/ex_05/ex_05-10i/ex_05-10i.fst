-- V exercise 10i

applier : (Int -> Int) -> Int -> Int
applier f x = f (f x)

plus1 : Int -> Int
plus1 x = x + 1

applyPlus1 : (Int -> Int)
applyPlus1 = applier plus1

main : Int
main = applyPlus1 10
-- result = 12