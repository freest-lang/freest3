-- I exercise 9

data IntList = Nil | List Int IntList

pow : Int -> Int -> Int
pow e n 
    | e <= 0    = 1
    | otherwise = n * (pow (e-1) n)

sumPowList : Int -> IntList -> Int
sumPowList e Nil = 0
sumPowList e (List x rest) = pow e x + (sumPowList e rest)

-- only positive integers

listTo : Int -> IntList -> IntList
listTo x list 
    | x <= 1    = List x list
    | otherwise = listTo (x-1) (List x list)
-- only positive integers

main : Int
main = sumPowList 2 (listTo 100 Nil)
-- result = 338350
