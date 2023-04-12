-- I exercise 9

data IntList = End | List Int IntList

sumPowList : Int -> IntList -> Int
sumPowList e list = 
    case list of {
        End -> 0,
        List x rest -> pow e x + (sumPowList e rest)
    }

pow : Int -> Int -> Int
pow e n = if e <= 0 
                then 1
                else n * (pow (e-1) n)
-- only positive integers

listTo : Int -> IntList -> IntList
listTo x list = if x <= 1
                    then List x list
                    else listTo (x-1) (List x list)
-- only positive integers

main : Int
main = sumPowList 2 (listTo 100 End)
-- result = 338350
