-- V exercise 12

data IntList = End | List Int IntList

concatLists : IntList -> IntList -> IntList
concatLists list1 list2 =
    case list1 of {
        End -> list2,
        List x rest -> List x (concatLists rest list2)
    }

list : IntList
list = List 1 (List 2 (List 3 End))

list12 : IntList
list12 = List 1 (List 2 End)

main : IntList
main = concatLists (concatLists (concatLists list12 list) list12) list
-- result = List 1 (List 2 (List 1 (List 2 (List 3 (List 1 (List 2 (List 1 (List 2 (List 3 End)))))))))
