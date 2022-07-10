-- IV exercise 1e

data IntList = End | List Int IntList

substitui : Int -> Int -> IntList -> IntList
substitui x y list = 
    case list of {
        End -> End,
        List z rest -> if x == z then List y (substitui x y rest) else List z (substitui x y rest)
    }

main : IntList
main = substitui 2 3 (List 1 (List 2 (List 3 End)))
-- result = List 1 (List 3 (List 3 End))
