-- V exercise 8c

data IntList = End | List Int IntList

map' : (Int -> Int) -> IntList -> IntList
map' f list =
    case list of {
        End -> End,
        List x rest -> List (f x) (map' f rest)
    }

sum' : IntList -> Int
sum' list =
    case list of {
        End -> 0,
        List x rest -> x + sum' rest
    }

generateFromTo : Int -> Int -> IntList
generateFromTo f t = if f > t then End else List f (generateFromTo (f+1) t) 

total : (Int -> Int) -> Int -> Int
total f i = sum' (map' f (generateFromTo 0 i))

plus1 : Int -> Int
plus1 x = x + 1

main : Int
main = total plus1 5
-- result = 21
