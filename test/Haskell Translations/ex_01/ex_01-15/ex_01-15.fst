-- I exercise 15

data IntList = IEnd | IList Int IntList
data PairList = End | List Int Int PairList

generate : IntList -> IntList -> PairList
generate list1 list2 = generate' list1 list2 list2          -- lazy correction for disappearing list, not scallable
                                                            -- should use indexes but it introduces many other problems, complicating easy resolution
generate' : IntList -> IntList -> IntList -> PairList
generate' list1 list2 list2Dup =
    case list1 of {
        IEnd -> End,
        IList x rest1 ->    case list2 of {
                                IEnd -> generate' rest1 list2Dup list2Dup,
                                IList y rest2-> List x y (generate' list1 rest2 list2Dup)    
                            }
    }

main : PairList
main = generate (IList 1 (IList 2 (IList 3 IEnd))) (IList 4 (IList 5 (IList 6 IEnd)))
-- List 1 4 (List 1 5 (List 1 6 (List 2 4 (List 2 5 (List 2 6 (List 3 4 (List 3 5 (List 3 6 End))))))))