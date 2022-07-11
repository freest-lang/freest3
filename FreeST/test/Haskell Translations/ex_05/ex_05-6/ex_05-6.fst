-- V exercise 6

data IntList = End | List Int IntList

dropWhile' : (Int -> Bool) -> IntList -> IntList
dropWhile' f list =
    case list of {
        End -> End,
        List x rest -> 
            if f x 
                then dropWhile' f rest
                else List x rest
    }

isEven : Int -> Bool
isEven x = mod x 2 == 0

list : IntList
list = List 4 (List 2 (List 6 (List 1 (List 8 (List 6 (List 2 End))))))

main : IntList
main = dropWhile' isEven list
-- result = List 1 (List 8 (List 6 (List 2 End)))
