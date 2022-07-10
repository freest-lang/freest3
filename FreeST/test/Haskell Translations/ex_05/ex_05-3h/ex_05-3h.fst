-- V exercise 3h

data IntList = End | List Int IntList

filter' : (Int -> Bool) -> IntList -> IntList
filter' f list = 
    case list of {
        End -> End,
        List x rest -> 
            if f x 
                then List x (filter' f rest)
                else filter' f rest
    }

map' : (Int -> Int) -> IntList -> IntList
map' f list =
    case list of {
        End -> End,
        List x rest -> List (f x) (map' f rest)
    }

isPositive : Int -> Bool
isPositive x = x > 0

pow2 : Int -> Int
pow2 x = x*x

list : IntList
list = List (-3) (List (-2) (List (-1) (List 0 (List 1 (List 2 (List 3 End))))))

main : IntList
main = filter' isPositive (map' pow2 list)
-- result =
