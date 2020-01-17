-- I exercise 5a

data IntList = End | List Int IntList

listLength : IntList -> Int
listLength list = 
    case list of {
        End -> 0,
        List _ rest -> 1 + listLength rest
    }

biggerThan10 : IntList -> Bool
biggerThan10 list = listLength list > 10

main : Bool
main = biggerThan10 (List 1 (List 2 End))
-- result = False