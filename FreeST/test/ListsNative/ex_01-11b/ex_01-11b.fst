-- I exercise 11b

sumList : [Int] -> Int
sumList list = 
    case list of {
        [] -> 0,
        x :: rest -> x + sumList rest
    }

getFacts : Int -> [Int]
getFacts x = getFacts' x (div x 2)
getFacts' : Int -> Int -> [Int]
getFacts' x d = if d <= 0 then [] else
                (if (mod x d) == 0
                    then d :: (getFacts' x (d-1))
                    else (getFacts' x (d-1))
                )

isPerfect: Int -> Bool
isPerfect x = sumList (getFacts x) == x

getPerfects : Int -> [Int]
getPerfects x = if x <= 1 then [] else
                    if isPerfect x 
                        then x :: (getPerfects (x-1))
                        else getPerfects (x-1)

main : [Int]
main = getPerfects 10000
-- result = 8128 :: 496 :: 28 :: 6 :: []