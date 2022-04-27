-- IV exercise 1m

fusao : [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
fusao list1 list2 = 
    case list1 of {
        [] -> list2,
        t1 :: rest1 ->
            let (k1,v1) = t1 in
            case list2 of {
                [] -> list1,
                t2 :: rest2 -> 
                    let (k2,v2) = t2 in
                    if k1 == k2 
                        then (k1,(v1+v2)) :: (fusao rest1 rest2) else
                    if k1 > k2
                        then (k2,v2) :: (fusao list1 rest2) else
                    -- if k1 < k2
                        (k1,v1) :: (fusao rest1 list2)
            }
    }

main : [(Int,Int)]
main = 
    let x = [(1,1),(2,1),(3,1),(4,1),(5,1),(6 ,1),(7 ,1)] in
    let y = [(1,1),(3,1),(5,1),(7,1),(9,1),(11,1),(13,1)] in
    fusao x y
-- result = [(1, 2),(2, 1),(3, 2),(4, 1),(5, 2),(6, 1),(7, 1),(9, 1),(11, 1),(13, 1)]