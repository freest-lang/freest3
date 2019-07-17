-- I exercise 17

data IntIntList = End | List Int Int IntIntList

pares : Int -> IntIntList
pares n = 
    let min = 1 in
    pares' min min min n

pares' : Int -> Int -> Int -> Int -> IntIntList
pares' i j min max = 
        if j > max 
            then pares' (i+1) min min max
            else if i > max
                    then End
                    else if i == j
                            then pares' i (j+1) min max
                            else List i j (pares' i (j+1) min max)

main : IntIntList
main = pares 10
-- result = List 1 2 (List 1 3 (List 1 4 (List 1 5 (List 1 6 (List 1 7 (List 1 8 (List 1 9 (List 1 10 (List 2 1 (List 2 3 (List 2 4 (List 2 5 (List 2 6 (List 2 7 (List 2 8 
-- (List 2 9 (List 2 10 (List 3 1 (List 3 2 (List 3 4 (List 3 5 (List 3 6 (List 3 7 (List 3 8 (List 3 9 (List 3 10 (List 4 1 (List 4 2 (List 4 3 (List 4 5 (List 4 6 
-- (List 4 7 (List 4 8 (List 4 9 (List 4 10 (List 5 1 (List 5 2 (List 5 3 (List 5 4 (List 5 6 (List 5 7 (List 5 8 (List 5 9 (List 5 10 (List 6 1 (List 6 2 (List 6 3 
-- (List 6 4 (List 6 5 (List 6 7 (List 6 8 (List 6 9 (List 6 10 (List 7 1 (List 7 2 (List 7 3 (List 7 4 (List 7 5 (List 7 6 (List 7 8 (List 7 9 (List 7 10 (List 8 1 
-- (List 8 2 (List 8 3 (List 8 4 (List 8 5 (List 8 6 (List 8 7 (List 8 9 (List 8 10 (List 9 1 (List 9 2 (List 9 3 (List 9 4 (List 9 5 (List 9 6 (List 9 7 (List 9 8 
-- (List 9 10 (List 10 1 (List 10 2 (List 10 3 (List 10 4 (List 10 5 (List 10 6 (List 10 7 (List 10 8 
-- (List 10 9 End)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))