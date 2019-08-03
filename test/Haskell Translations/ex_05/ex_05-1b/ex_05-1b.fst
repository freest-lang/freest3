-- V exercise 1b

isBiggerThan0 : Int -> Bool
isBiggerThan0 x = x > 0

main : Bool
main = isBiggerThan0 (-10)
-- result = False