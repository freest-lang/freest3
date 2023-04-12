-- I exercise 3

data MaybeInt = Empty | Number Int
data IntList = End | List Int IntList
-- data List a = End | List a List (for generic types)

-- without pattern matching
takeHead : IntList -> MaybeInt
takeHead list =
    case list of {
        End -> Empty,
        List x _ -> Number x
    }

-- with pattern matching
-- takeHeadPM : [a] -> a
-- takeHeadPM (x:_) = a

main : MaybeInt
main = takeHead (List 1 (List 2 (List 3 End)))
-- main = takeHeadPM [1,2,3]

-- result = Number 1
