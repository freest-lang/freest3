-- I exercise 5i

data IntList = End | List Int IntList
data MaybeInt = Empty | Number Int

isSameLengthAndHasSameLast : IntList -> IntList -> Bool
isSameLengthAndHasSameLast list1 list2 = (isSameLength list1 list2) && (hasSameLast list1 list2)

isSameLength : IntList -> IntList -> Bool
isSameLength list1 list2 = (listLength list1) == (listLength list2)

listLength : IntList -> Int
listLength list = 
    case list of {
        End -> 0,
        List _ rest -> 1 + listLength rest
    }

hasSameLast : IntList -> IntList -> Bool
hasSameLast list1 list2 = equalsMaybeInt (getLast list1) (getLast list2)

getLast : IntList -> MaybeInt
getLast list = 
    case list of {
        End -> Empty,
        List x rest ->  case rest of {
                            End -> Number x,
                            List _ _ -> getLast rest
                        }
    }

equalsMaybeInt : MaybeInt -> MaybeInt -> Bool
equalsMaybeInt a b =
    case a of {
        Empty ->    case b of {
                        Empty -> True,
                        Number _ -> False    
                    },
        Number x ->   case b of {
                        Empty -> False,
                        Number y -> x == y
                    }
    }

main : Bool
main = isSameLengthAndHasSameLast (List 1 (List 2 (List 3 End))) (List 2 (List 1 (List 3 End)))
-- result True