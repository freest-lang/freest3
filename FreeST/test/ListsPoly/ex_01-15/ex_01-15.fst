-- I exercise 15

generate : [Int] -> [Int] -> [(Int,Int)]
generate list1 list2 = generate' list1 list2 list2          -- lazy correction for disappearing list, not scallable
                                                            -- should use indexes but it introduces many other problems, complicating easy resolution
generate' : [Int] -> [Int] -> [Int] -> [(Int,Int)]
generate' list1 list2 list2Dup =
    case list1 of {
        [] -> [],
        x :: rest1 ->    
            case list2 of {
                []         -> generate' rest1 list2Dup list2Dup,
                y :: rest2 -> (x,y) :: (generate' list1 rest2 list2Dup)    
            }
    }

main : [(Int,Int)]
main = generate  ([1,2,3]) ([4,5,6])
-- [(1, 4),(1, 5),(1, 6),(2, 4),(2, 5),(2, 6),(3, 4),(3, 5),(3, 6)]