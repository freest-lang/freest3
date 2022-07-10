-- V exercise 10g

main : Int 
main = (\x : Int -> (\y : Int -> x + y) ) 10 20
-- result = 30
