-- V exercise 20

data IntList = End | List Int IntList

indexOf : Int -> IntList -> Int 
indexOf e list = 
    case list of {
        End -> -1,
        List x rest -> 
            if e == x 
                then 0
                else 1 + (indexOf e rest)
    }

list : IntList
list = List 1 (List 2 (List 3 (List 4 (List 5 End))))

main : Int
main = indexOf 4 list
--result = 3