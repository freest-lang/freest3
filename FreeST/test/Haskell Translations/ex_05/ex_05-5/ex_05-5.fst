-- V exercise 5

data IntList = End | List Int IntList

takeWhile' : (Int -> Bool) -> IntList -> IntList
takeWhile' f list = 
    case list of {
        End -> End,
        List x rest -> 
            if f x 
                then List x (takeWhile' f rest)
                else End
    }

isEven : Int -> Bool
isEven x = mod x 2 == 0

list : IntList
list = List 4 (List 2 (List 6 (List 1 (List 8 (List 6 (List 2 End))))))

main : IntList
main = takeWhile' isEven list
-- result = List 4 (List 2 (List 6 End))
