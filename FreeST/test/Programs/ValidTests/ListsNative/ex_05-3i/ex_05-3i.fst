-- V exercise 3i

map' : (Int -> Int) -> [Int] -> [Int]
map' f list =
    case list of {
        [] -> [],
        x :: rest -> (f x) :: (map' f rest)
    }

filter' : (Int -> Bool) -> [Int] -> [Int]
filter' f list =
    case list of {
        [] -> [],
        x :: rest -> 
            if f x
                then x :: (filter' f rest)
                else filter' f rest
    }

pow2 : Int -> Int
pow2 x = x*x

isPositive : Int -> Bool
isPositive x = x > 0

list : [Int]
list = [-3,-2,-1,0,1,2,3]

main : [Int]
main = map' pow2 (filter' isPositive list)
-- result = [1,4,9]