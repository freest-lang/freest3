-- V exercise 6

dropWhile' : (Int -> Bool) -> [Int] -> [Int]
dropWhile' f list =
    case list of {
        [] -> [],
        x :: rest -> 
            if f x 
                then dropWhile' f rest
                else x :: rest
    }

isEven : Int -> Bool
isEven x = mod x 2 == 0

list : [Int]
list = [4,2,6,1,8,6,2]

main : [Int]
main = dropWhile' isEven list
-- result = [1,8,6,2]