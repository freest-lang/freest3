-- V exercise 7

data IntList = End | List Int IntList

dropUntil' : (Int -> Bool) -> IntList -> IntList
dropUntil' f list =
    case list of {
        End -> End,
        List x rest -> 
            if f x 
                then List x rest
                else dropUntil' f rest
    }

isPositive : Int -> Bool
isPositive x = x > 0

list : IntList
list = List (-4) (List 0 (List (-8) (List 3 (List (-2) (List (-5) (List 3 End))))))

main : IntList 
main = dropUntil' isPositive list
-- result = List 3 (List (-2) (List (-5) (List 3 End)))
