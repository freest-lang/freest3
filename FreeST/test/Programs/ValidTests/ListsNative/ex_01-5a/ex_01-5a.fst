-- I exercise 5a

listLength : [Int] -> Int
listLength list = 
    case list of {
        []        -> 0,
        _ :: rest -> 1 + listLength rest
    }

biggerThan10 : [Int] -> Bool
biggerThan10 list = listLength list > 10

main : Bool
main = biggerThan10 ([1,2])
-- result = False