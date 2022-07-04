-- I exercise 9

data IntList = End | List Int IntList

sumPowList : Int -> IntList -> Int
sumPowList e End = 0
sumPowList e (List x rest) = pow e x + (sumPowList e rest)

pow : Int -> Int -> Int
pow e n 
    | e <= 0    = 1
    | otherwise = n * (pow (e-1) n)
-- only positive integers

listTo : Int -> IntList -> IntList
listTo x list 
    | x <= 1    = List x list
    | otherwise = listTo (x-1) (List x list)
-- only positive integers

main : Int
main = sumPowList 2 (listTo 100 End)
-- result = 338350