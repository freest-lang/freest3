-- I exercise 5g

data IntList = End | List Int IntList

invertAllButFirst : IntList -> IntList
invertAllButFirst list = 
    case list of {
        End -> End,
        List x rest -> List x (invert rest)
    }

invert : IntList -> IntList 
invert list = invert' list End

invert' : IntList -> IntList -> IntList
invert' from to = 
    case from of {
        End -> to,
        List x rest -> invert' rest (List x to)
    }

main : IntList
main = invertAllButFirst (List 1 (List 2 (List 3 (List 4 (List 5 End)))))
-- result = List 1 (List 5 (List 4 (List 3 (List 2 End))))
