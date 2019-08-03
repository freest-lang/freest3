-- V exercise 3g

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

isEven : Int -> Bool
isEven x = mod x 2 == 0

list : IntList
list = List 1 (List 2 (List 3 (List 4 (List 5 (List 6 
       (List 7 (List 8 (List 9 (List 10 End)))))))))

main : IntList
main = filter' isEven list
-- result = List 2 (List 4 (List 6 (List 8 (List 10 End))))