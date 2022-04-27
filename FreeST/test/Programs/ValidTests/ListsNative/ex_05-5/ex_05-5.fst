-- V exercise 5

takeWhile' : (Int -> Bool) -> [Int] -> [Int]
takeWhile' f list = 
    case list of {
        [] -> [],
        x :: rest -> 
            if f x 
                then x :: (takeWhile' f rest)
                else []
    }

isEven : Int -> Bool
isEven x = mod x 2 == 0

list : [Int]
list = [4,2,6,1,8,6,3]

main : [Int]
main = takeWhile' isEven list
-- result = [4,2,6]