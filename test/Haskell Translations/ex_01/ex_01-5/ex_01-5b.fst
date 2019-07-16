-- I exercise 5b

data IntList = End | List Int IntList

isNull : IntList -> Bool
isNull list = 
    case list of {
        End -> True,
        List _ _ -> False
    }

isNotNull : IntList -> Bool
isNotNull list = not (isNull list)

main : Bool
main = isNotNull (List 1 End)
-- result = True