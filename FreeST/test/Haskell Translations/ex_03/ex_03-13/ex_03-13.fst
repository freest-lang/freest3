-- I exercise 13

-- zip xs ys
--  where xs = tail [0,1,2,3,4]
--        ys = init ['a','b','c','d']

data IntList = IEnd | IList Int IntList
data CharList = CEnd | CList Char CharList

tailIntList : IntList -> IntList
tailIntList list = 
    case list of {
        IEnd -> IEnd,
        IList _ rest -> rest
    }

initCharList : CharList -> CharList
initCharList list =
    case list of {
        CEnd -> CEnd,
        CList x rest -> case rest of {
                            CEnd -> CEnd,
                            CList _ _ -> CList x (initCharList rest)                
                        }
    }

data Pair = Pair Int Char
data PairList = End | List Pair PairList

zipList : IntList -> CharList -> PairList
zipList list1 list2 = 
    case list1 of {
        IEnd -> End,
        IList x rest1 -> case list2 of {
                            CEnd -> End,
                            CList c rest2 -> List (Pair x c) (zipList rest1 rest2)
                        }
    }

main : PairList
main = zipList  (tailIntList (IList 0 (IList 1 (IList 2 (IList 3 IEnd)))))
                (initCharList (CList 'a' (CList 'b' (CList 'c' CEnd))))
-- result = List (Pair 1 'a') (List (Pair 2 'b') End)