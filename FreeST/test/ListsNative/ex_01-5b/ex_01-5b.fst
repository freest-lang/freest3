-- I exercise 5b

isNull : [Int] -> Bool
isNull list = 
    case list of {
        []     -> True,
        _ :: _ -> False
    }

isNotNull : [Int] -> Bool
isNotNull list = not (isNull list)

main : Bool
main = isNotNull [1]
-- result = True