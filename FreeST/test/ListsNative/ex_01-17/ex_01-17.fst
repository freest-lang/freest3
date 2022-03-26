-- I exercise 17

pares : Int -> [(Int,Int)]
pares n = 
    let min = 1 in
    pares' min min min n

pares' : Int -> Int -> Int -> Int -> [(Int,Int)]
pares' i j min max = 
        if j > max 
            then pares' (i+1) min min max
            else if i > max
                    then []
                    else if i == j
                            then pares' i (j+1) min max
                            else (i,j) :: (pares' i (j+1) min max)

main : [(Int,Int)]
main = pares 10
-- result = (1,2) :: (1,3) :: (1,4) :: (1,5) :: (1,6) :: (1,7) :: (1,8) :: (1,9) :: (1,10) :: (2,1) :: (2,3) :: (2,4) :: (2,5) :: (2,6) :: (2,7) :: (2,8) :: (2,9) :: (2,10) :: (3,1) :: (3,2) :: (3,4) :: (3,5) :: (3,6) :: (3,7) :: (3,8) :: (3,9) :: (3,10) :: (4,1) :: (4,2) :: (4,3) :: (4,5) :: (4,6) :: (4,7) :: (4,8) :: (4,9) :: (4,10) :: (5,1) :: (5,2) :: (5,3) :: (5,4) :: (5,6) :: (5,7) :: (5,8) :: (5,9) :: (5,10) :: (6,1) :: (6,2) :: (6,3) :: (6,4) :: (6,5) :: (6,7) :: (6,8) :: (6,9) :: (6,10) :: (7,1) :: (7,2) :: (7,3) :: (7,4) :: (7,5) :: (7,6) :: (7,8) :: (7,9) :: (7,10) :: (8,1) :: (8,2) :: (8,3) :: (8,4) :: (8,5) :: (8,6) :: (8,7) :: (8,9) :: (8,10) :: (9,1) :: (9,2) :: (9,3) :: (9,4) :: (9,5) :: (9,6) :: (9,7) :: (9,8) :: (9,10) :: (10,1) :: (10,2) :: (10,3) :: (10,4) :: (10,5) :: (10,6) :: (10,7) :: (10,8) :: (10,9) :: []