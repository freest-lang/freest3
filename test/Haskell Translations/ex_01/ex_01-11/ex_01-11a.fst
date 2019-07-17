-- I exercise 11a

data IntList = End | List Int IntList

getFacts : Int -> IntList
getFacts x = getFacts' x (div x 2)
getFacts' : Int -> Int -> IntList
getFacts' x d = if d <= 0 then End else
                (if (mod x d) == 0
                    then List d (getFacts' x (d-1))
                    else (getFacts' x (d-1))
                )

main : IntList
main = getFacts 10000
-- result List 5000 (List 2500 (List 2000 (List 1250 (List 1000 (List 625 
-- (List 500 (List 400 (List 250 (List 200 (List 125 (List 100 (List 80 
-- (List 50 (List 40 (List 25 (List 20 (List 16 (List 10 (List 8 
-- (List 5 (List 4 (List 2 (List 1 End)))))))))))))))))))))))