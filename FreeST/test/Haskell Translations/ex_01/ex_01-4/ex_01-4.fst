-- I exercise 4

data IntList Int = End | List Int IntList

listLength : IntList -> Int
listLength list = 
    case list of {
        End -> 0,
        List _ rest -> 1 + listLength rest
    }

main : Int
main = listLength (List 1 (List 2 (List 3 End)))
-- result = 3
