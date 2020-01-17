-- IV exercise 1f

data IntList = End | List Int IntList

altera : IntList -> Int -> Int -> IntList
altera list x y = 
    case list of {
        End -> End,
        List z rest -> if z < x 
                            then List y (altera rest x y)
                            else List z (altera rest x y)
    }

main : IntList
main = altera (List 10 (List 0 (List 23 (List 4 (List 14 (List 2 (List 11 End))))))) 10 5
-- result = List 10 (List 5 (List 23 (List 5 (List 14 (List 5 (List 11 End))))))