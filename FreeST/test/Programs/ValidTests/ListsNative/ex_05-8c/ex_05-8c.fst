-- V exercise 8c

map' : (Int -> Int) -> [Int] -> [Int]
map' f list =
    case list of {
        [] -> [],
        x :: rest -> (f x) :: (map' f rest)
    }

sum' : [Int] -> Int
sum' list =
    case list of {
        [] -> 0,
        x :: rest -> x + sum' rest
    }

generateFromTo : Int -> Int -> [Int]
generateFromTo f t = 
    if f > t 
        then []
        else f :: (generateFromTo (f+1) t) 

total : (Int -> Int) -> Int -> Int
total f i = sum' (map' f (generateFromTo 0 i))

plus1 : Int -> Int
plus1 x = x + 1

main : Int
main = total plus1 5
-- result = 21