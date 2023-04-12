-- IV exercise 1g

data IntList = End | List Int IntList

multiplos : IntList -> Int -> IntList
multiplos list x = 
    case list of {
        End -> End,
        List y rest -> if mod y x == 0
                            then List y (multiplos rest x)
                            else multiplos rest x
    }

main : IntList
main = multiplos (List 1 (List 3 (List 6 (List 2 (List 5 (List 15 (List 3 (List 5 End)))))))) 3
-- result = List 3 (List 6 (List 15 (List 3 End)))
