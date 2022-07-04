-- I exercise 11b

data IntList = End | List Int IntList

sumList : IntList -> Int
sumList End           = 0
sumList (List x rest) = x + sumList rest

getFacts : Int -> IntList
getFacts x = getFacts' x (div x 2)

getFacts' : Int -> Int -> IntList
getFacts' x d 
    | d <= 0       = End 
    | mod x d == 0 = List d (getFacts' x (d-1))
    | otherwise    = (getFacts' x (d-1))

isPerfect: Int -> Bool
isPerfect x = sumList (getFacts x) == x

getPerfects : Int -> IntList
getPerfects x 
    | x <= 1      = End
    | isPerfect x = List x (getPerfects (x-1))
    | otherwise   = getPerfects (x-1)

main : IntList
main = getPerfects 50
-- result = List 28 (List 6 End)