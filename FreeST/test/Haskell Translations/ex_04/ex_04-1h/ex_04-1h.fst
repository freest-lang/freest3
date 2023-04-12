-- IV exercise 1h

data Pair = Pair Int Int
data IntList = IEnd | IList Int IntList
data IntPairList = End | List Pair IntPairList

zip' : IntList -> IntList -> IntPairList
zip' list1 list2 =
    case list1 of {
        IEnd -> End,
        IList x1 rest1 -> case list2 of {
                                IEnd -> End,
                                IList x2 rest2 -> List (Pair x1 x2) (zip' rest1 rest2)
                            }
    }

main : IntPairList
main = zip' (IList 1 (IList 2 (IList 3 IEnd))) (IList 3 (IList 2 (IList 1 IEnd)))
-- result = List (Pair 1 3) (List (Pair 2 2) (List (Pair 3 1) End))
