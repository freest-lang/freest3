-- V exercise 3a

map' : (Int -> Int) -> [Int] -> [Int]
map' f list = 
    case list of {
        [] -> [],
        x :: rest -> (f x) :: (map' f rest)
    }

sum1 : Int -> Int
sum1 x = x + 1

list : [Int]
list = [1,2,3,4,5,6,7]

main : [Int]
main = map' sum1 list
-- result = [2,3,4,5,6,7,8]