-- IV exercise 1d

data IntList = End | List Int IntList

elem' : Int -> IntList -> Bool
elem' x list = 
    case list of {
        End -> False,
        List y rest -> if x == y then True else elem' x rest
    }

main : Bool
main = elem' 3 (List 1 (List 2 (List 3 (List 4 End))))
-- result = True