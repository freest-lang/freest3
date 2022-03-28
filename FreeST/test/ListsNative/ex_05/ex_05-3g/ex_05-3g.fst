-- V exercise 3g

filter' : (Int -> Bool) -> [Int] -> [Int]
filter' f list = 
    case list of {
        [] -> [],
        x :: rest -> 
            if f x 
                then x :: (filter' f rest)
                else filter' f rest
    }

isEven : Int -> Bool
isEven x = mod x 2 == 0

list : [Int]
list = [1,2,3,4,5,6,7,8,9,10]

main : [Int]
main = filter' isEven list
-- result = [2,4,6,8,10]