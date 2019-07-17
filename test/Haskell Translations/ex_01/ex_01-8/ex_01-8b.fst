-- I exercise 8b

data IntList = End | List Int IntList

multiplyBy2 : IntList -> IntList
multiplyBy2 list = 
    case list of {
        End -> End,
        List x rest -> List (x*x) (multiplyBy2 rest)
    }

main : IntList
main = multiplyBy2 (List 1 (List 2 (List 3 End)))
-- result List 1 (List 4 (List 9 End))