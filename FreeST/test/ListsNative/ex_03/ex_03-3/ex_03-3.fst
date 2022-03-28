-- I exercise 3

-- data List a = End | List a List (for generic types)

-- without pattern matching
takeHead : [Int] -> Int
takeHead list =
    case list of {
        x :: _ -> x
    }

-- with pattern matching
-- takeHeadPM : [a] -> a
-- takeHeadPM (x:_) = a

main : Int
main = takeHead ([1,2,3])
-- main = takeHeadPM [1,2,3]

-- result = 1