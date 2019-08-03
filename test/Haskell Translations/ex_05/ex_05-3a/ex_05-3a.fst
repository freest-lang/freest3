-- V exercise 3a

data IntList = End | List Int IntList

map' : (Int -> Int) -> IntList -> IntList
map' f list = 
    case list of {
        End -> End,
        List x rest -> List (f x) (map' f rest)
    }

sum1 : Int -> Int
sum1 x = x + 1

list : IntList
list = List 1 (List 2 (List 3 (List 4 (List 5 (List 6 (List 7 End))))))

main : IntList
main = map' sum1 list
-- result = List 2 (List 3 (List 4 (List 5 (List 6 (List 7 (List 8 End))))))