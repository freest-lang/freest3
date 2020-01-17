-- I exercise 9

data IntList = End | List Int IntList
data PairIntList = Pair IntList IntList

halve : IntList -> PairIntList
halve list = 
    let x = div (lengthList list) 2 in
    Pair (takeList x list) (dropList x list)

lengthList : IntList -> Int
lengthList list = 
    case list of {
        End -> 0,
        List _ rest -> 1 + lengthList rest
    }

takeList : Int -> IntList -> IntList
takeList i list = 
    case list of {
        End -> End,
        List n rest -> if i == 0 then End else List n (takeList (i-1) rest)
    }

dropList : Int -> IntList -> IntList
dropList i list = 
    case list of {
        End -> End,
        List n rest -> if i == 0 then List n rest else dropList (i-1) rest
    }

main : PairIntList
main = halve (List 1 (List 2 (List 3 (List 4 (List 5 (List 6 End))))))