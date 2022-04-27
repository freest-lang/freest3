-- V exercise 3b

map' : (Int -> Bool) -> [Int] -> [Bool]
map' f list = 
    case list of {
        [] -> [],
        x :: rest -> (f x) :: (map' f rest)
    }

isBiggerThen0 : Int -> Bool
isBiggerThen0 x = x > 0

list : [Int]
list = [1,2,3,4,5,6,-1]

main : [Bool]
main = map' isBiggerThen0 list
-- result = [True,True,True,True,True,True,False]