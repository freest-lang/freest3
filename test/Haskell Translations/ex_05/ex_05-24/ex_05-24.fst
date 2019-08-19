-- V exercise 24

iter : (Int -> Int) -> Int -> (Int -> Int)
iter f i = 
    if i <= 1
        then f
        else (\x : Int -> iter f (i-1) (f x))

main : Int
main = iter (\x : Int -> x + 1) 5 10
--result = 15