-- I exercise 5i

data MaybeInt = Empty | Number Int

isSameLengthAndHasSameLast : [Int] -> [Int] -> Bool
isSameLengthAndHasSameLast list1 list2 = (isSameLength list1 list2) && (hasSameLast list1 list2)

isSameLength : [Int] -> [Int] -> Bool
isSameLength list1 list2 = (listLength list1) == (listLength list2)

listLength : [Int] -> Int
listLength list = 
    case list of {
        [] -> 0,
        _ :: rest -> 1 + listLength rest
    }

hasSameLast : [Int] -> [Int] -> Bool
hasSameLast list1 list2 = equalsMaybeInt (getLast list1) (getLast list2)

getLast : [Int] -> MaybeInt
getLast list = 
    case list of {
        [] -> Empty,
        x :: rest ->  
            case rest of {
                []     -> Number x,
                _ :: _ -> getLast rest
            }
    }

equalsMaybeInt : MaybeInt -> MaybeInt -> Bool
equalsMaybeInt a b =
    case a of {
        Empty ->    
            case b of {
                Empty    -> True,
                Number _ -> False    
            },
        Number x ->   
            case b of {
                Empty    -> False,
                Number y -> x == y
            }
    }

main : Bool
main = isSameLengthAndHasSameLast ([1,2,3]) ([2,1,3])
-- result True