-- IV exercise 1l

data IntList = End | List Int IntList

trocaPares : IntList -> IntList
trocaPares list = 
    case list of {
        End -> End,
        List x rest1 -> case rest1 of {
                            End -> List x rest1,
                            List y rest2 -> List y (List x (trocaPares rest2))
                        }
    }

main : IntList
main = trocaPares (List 1 (List 2 (List 3 (List 4 (List 5 End)))))
-- result = List 2 (List 1 (List 4 (List 3 (List 5 End))))