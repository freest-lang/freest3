-- I exercise 8

data IntList = End | List Int IntList

multiplyBy2 : IntList -> IntList
multiplyBy2 list = 
    case list of {
        End -> End,
        List x rest -> List (2*x) (multiplyBy2 rest)
    }

main : IntList
main = multiplyBy2 (List 1 (List 2 (List 3 End)))
-- result List 2 (List 4 (List 6 End))
