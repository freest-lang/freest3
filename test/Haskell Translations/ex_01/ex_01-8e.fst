-- I exercise 8e

data    IntList =   IEnd |  IList Int        IntList
data IntIntList =  IIEnd | IIList Int Int IntIntList

isOdd : Int -> Bool
isOdd x = not ((mod x 2) == 0)

zipWhen : (Int -> Bool) -> IntList -> IntList ->  IntIntList
zipWhen f list1 list2 = 
    case list1 of {
        IEnd -> IIEnd,
        IList x rest -> keepIf f (concatIIList (zipIt x list2) (zipWhen f rest list2))
    }

zipIt : Int -> IntList -> IntIntList
zipIt x list = 
    case list of {
        IEnd -> IIEnd,
        IList y rest -> IIList x y (zipIt x rest)
    }

concatIIList : IntIntList -> IntIntList -> IntIntList
concatIIList list1 list2 = 
    case list1 of {
        IIEnd -> list2,
        IIList x y rest1 -> case rest1 of {
                                IIEnd -> IIList x y list2,
                                IIList _ _ rest2 -> IIList x y (concatIIList rest1 list2)
                            } 
    }

keepIf : (Int -> Bool) -> IntIntList -> IntIntList
keepIf f list =
    case list of {
        IIEnd -> IIEnd,
        IIList x y rest -> if f x 
                                then IIList x y (keepIf f rest)
                                else keepIf f rest
    }

main : IntIntList
main = zipWhen isOdd (IList 1 (IList 2 (IList 3 IEnd))) (IList 1 (IList 2 (IList 3 IEnd)))
-- result = IIList 1 1 (IIList 1 2 (IIList 1 3 (IIList 3 1 (IIList 3 2 (IIList 3 3 IIEnd)))))