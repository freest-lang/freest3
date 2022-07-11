-- IV exercise 1m

data PairList = End | List Int Int PairList

fusao : PairList -> PairList -> PairList
fusao list1 list2 = 
    case list1 of {
        End -> list2,
        List k1 v1 rest1 -> 
            case list2 of {
                End -> list1,
                List k2 v2 rest2 -> 
                    if k1 == k2 
                        then List k1 (v1+v2) (fusao rest1 rest2) else
                    if k1 > k2
                        then List k2 v2 (fusao list1 rest2) else
                    -- if k1 < k2
                        List k1 v1 (fusao rest1 list2)
            }
    }

main : PairList
main = 
    let x = List 1 1 (List 2 1 (List 3 1 (List 4 1 (List 5 1 (List 6 1 (List 7 1 End))))))      in
    let y = List 1 1 (List 3 1 (List 5 1 (List 7 1 (List 9 1 (List 11 1 (List 13 1 End))))))    in
    fusao x y
-- result = List 1 2 (List 2 1 (List 3 2 (List 4 1 (List 5 2 (List 6 1 
--          (List 7 2 (List 9 1 (List 11 1 (List 13 1 End)))))))))
