-- IV exercise 1h

zip' : [Int] -> [Int] -> [(Int,Int)]
zip' list1 list2 =
    case list1 of {
        [] -> [],
        x1 :: rest1 -> 
            case list2 of {
                [] -> [],
                x2 :: rest2 -> (x1,x2) :: (zip' rest1 rest2)
            }
    }

main : [(Int,Int)]
main = zip' ([1,2,3]) ([3,2,1])
-- result = [(1,3),(2,2),(3,1)]