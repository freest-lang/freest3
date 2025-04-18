-- IV exercise 1i

data IntList = End | List Int IntList

potencias : Int -> IntList -> IntList
potencias b exp =
    case exp of {
        End -> End,
        List e rest -> List (pow b e) (potencias b rest)
    }

pow : Int -> Int -> Int
pow b e = if e == 0 then 1 else b * (pow b (e-1))

main : IntList
main = potencias 3 (List 1 (List 2 (List 3 (List 4 (List 5 (List 6 (List 7 (List 8 (List 9 (List 10 End ))))))))))
-- result = List 3 (List 9 (List 27 (List 81 (List 243 (List 729 
--          (List 2187 (List 6561 (List 19683 (List 59049 End)))))))))
