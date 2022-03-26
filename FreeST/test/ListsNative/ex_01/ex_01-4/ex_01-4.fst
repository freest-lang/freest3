-- I exercise 4

listLength : [Int] -> Int
listLength list = 
    case list of {
        []        -> 0,
        e :: rest -> 1 + listLength rest
    }

main : Int
main = listLength [1,2,3]
-- result = 3