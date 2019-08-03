-- V exercise 3i

data IntList = End | List Int IntList

map' : (Int -> Int) -> IntList -> IntList
map' f list =
    case list of {
        End -> End,
        List x rest -> List (f x) (map' f rest)
    }

filter' : (Int -> Bool) -> IntList -> IntList
filter' f list =
    case list of {
        End -> End,
        List x rest -> 
            if f x
                then List x (filter' f rest)
                else filter' f rest
    }

pow2 : Int -> Int
pow2 x = x*x

isPositive : Int -> Bool
isPositive x = x > 0

list : IntList
list = List (-3) (List (-2) (List (-1) (List 0 (List 1 (List 2 (List 3 End))))))

main : IntList
main = map' pow2 (filter' isPositive list)
-- result = List 1 (List 4 (List 9 End))