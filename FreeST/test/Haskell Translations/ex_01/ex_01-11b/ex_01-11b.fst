-- I exercise 11b

data IntList = End | List Int IntList

sumList : IntList -> Int
sumList list = 
    case list of {
        End -> 0,
        List x rest -> x + sumList rest
    }

getFacts : Int -> IntList
getFacts x = getFacts' x (div x 2)
getFacts' : Int -> Int -> IntList
getFacts' x d = if d <= 0 then End else
                (if (mod x d) == 0
                    then List d (getFacts' x (d-1))
                    else (getFacts' x (d-1))
                )

isPerfect: Int -> Bool
isPerfect x = sumList (getFacts x) == x

getPerfects : Int -> IntList
getPerfects x = if x <= 1 then End else
                    if isPerfect x 
                        then List x (getPerfects (x-1))
                        else getPerfects (x-1)

main : IntList
main = getPerfects 10000
-- result = List 8128 (List 496 (List 28 (List 6 End)))
