-- IV exercise 1c

data MaybeInt = Empty | Number Int
data IntList = End | List Int IntList

maximo : IntList -> MaybeInt
maximo list = 
    case list of {
        End -> Empty,
        List x rest -> maximo' x rest
    }

maximo' : Int -> IntList -> MaybeInt
maximo' max list = 
    case list of {
        End -> Number max,
        List x rest -> if max > x
                            then maximo' max rest
                            else maximo' x rest
    }

main : MaybeInt
main = maximo (List 1 (List 2 (List 5 (List 3 (List 4 End)))))
-- result = Number 5
