-- I exercise 9

sumPowList : Int -> [Int] -> Int
sumPowList e list = 
    case list of {
        [] -> 0,
        x :: rest -> pow e x + (sumPowList e rest)
    }

pow : Int -> Int -> Int
pow e n = 
    if e <= 0 
        then 1
        else n * (pow (e-1) n)
-- only positive integers

listTo : Int -> [Int] -> [Int]
listTo x list = 
    if x <= 1
        then x :: list
        else listTo (x-1) (x :: list)
-- only positive integers

main : Int
main = sumPowList 2 (listTo 100 [])
-- result = 338350