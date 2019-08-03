-- V exercise 3f

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

biggerThan : Int -> Int -> Bool
biggerThan x y = x < y

list : IntList
list = List 1 (List 2 (List 3 (List 4 (List 5 (List 6 End)))))

main : IntList
main = filter' (biggerThan 5) list
-- result = List 6 End