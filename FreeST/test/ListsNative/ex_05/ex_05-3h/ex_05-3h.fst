-- V exercise 3h

filter' : (Int -> Bool) -> [Int] -> [Int]
filter' f list = 
    case list of {
        [] -> [],
        x :: rest -> 
            if f x 
                then x :: (filter' f rest)
                else filter' f rest
    }

map' : (Int -> Int) -> [Int] -> [Int]
map' f list =
    case list of {
        [] -> [],
        x :: rest -> (f x) :: (map' f rest)
    }

isPositive : Int -> Bool
isPositive x = x > 0

pow2 : Int -> Int
pow2 x = x*x

list : [Int]
list = [-3,-2,-1,0,1,2,3]

main : [Int]
main = filter' isPositive (map' pow2 list)
-- result = [9,4,1,1,4,9]