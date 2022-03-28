-- V exercise 3f

filter' : (Int -> Bool) -> [Int] -> [Int]
filter' f list =
    case list of {
        [] -> [],
        x :: rest -> 
            if f x 
                then x :: (filter' f rest)
                else filter' f rest
    }

biggerThan : Int -> Int -> Bool
biggerThan x y = x < y

list : [Int]
list = [1,2,3,4,5,6]

main : [Int]
main = filter' (biggerThan 5) list
-- result = [6]