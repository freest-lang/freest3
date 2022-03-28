-- V exercise 7

dropUntil' : (Int -> Bool) -> [Int] -> [Int]
dropUntil' f list =
    case list of {
        [] -> [],
        x :: rest -> 
            if f x 
                then x :: rest
                else dropUntil' f rest
    }

isPositive : Int -> Bool
isPositive x = x > 0

list : [Int]
list = [-4,0,-8,3,-2,-5,3]

main : [Int] 
main = dropUntil' isPositive list
-- result = [3,-2,-5,3]