-- V exercise 8a

total : (Int -> Int) -> Int -> Int
total f i = if i == 0 then f i else f i + total f (i-1)

plus1 : Int -> Int
plus1 x = x + 1

main : Int
main = total plus1 5
-- result = 21