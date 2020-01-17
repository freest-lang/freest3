-- VIII exercise 2b

-- adapted for number so it can be compiled

data List = End | List Int List
data ListList = LE | LL List ListList
data BoolList = BE | BL Bool BoolList

palindrome : List -> Bool
palindrome list = equals list (reverseList list)

reverseList : List -> List
reverseList list = reverseList' list End

reverseList' : List -> List -> List
reverseList' s1 s2 = 
    case s1 of {
        End -> s2,
        List n rest -> reverseList' rest (List n s2)
    }

equals : List -> List -> Bool
equals s1 s2 =
    case s1 of {
        End -> 
            case s2 of {
                End -> True,
                List _ _ -> False
            },
        List n1 r1 -> 
            case s2 of {
                End -> False,
                List n2 r2 -> 
                    if n1 /= n2         -- Char's cannot be tested equal
                        then False
                        else equals r1 r2
            }
    }

number1 : List
number1 = List 1 (List 2 (List 3 (List 4 (List 5 (List 6 (List 7 (List 8 (List 9 (List 10 End)))))))))

number2 : List
number2 = List 1 (List 2 (List 3 (List 4 (List 3 (List 2 (List 1 End))))))

map' : (List -> Bool) -> ListList -> BoolList
map' f list =
    case list of {
        LE -> BE,
        LL s rest -> BL (f s) (map' f rest) 
    }

main : BoolList
main = map' palindrome (LL number1 (LL number2 LE))
--result = BL False (BL True BE)