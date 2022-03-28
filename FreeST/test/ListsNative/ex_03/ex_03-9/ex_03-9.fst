-- I exercise 9

halve : [Int] -> ([Int],[Int]]
halve list = 
    let x = div (lengthList list) 2 in
    ((takeList x list),(dropList x list))

lengthList : [Int] -> Int
lengthList list = 
    case list of {
        [] -> 0,
        _ :: rest -> 1 + lengthList rest
    }

takeList : Int -> [Int] -> [Int]
takeList i list = 
    case list of {
        [] -> [],
        n :: rest -> 
            if i == 0 
                then [] 
                else n :: (takeList (i-1) rest)
    }

dropList : Int -> [Int] -> [Int]
dropList i list = 
    case list of {
        [] -> [],
        n :: rest -> 
            if i == 0 
                then n :: rest 
                else dropList (i-1) rest
    }

main : ([Int],[Int])
main = halve ([1,2,3,4,5,6])