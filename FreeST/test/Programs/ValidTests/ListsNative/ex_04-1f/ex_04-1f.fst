-- IV exercise 1f

altera : [Int] -> Int -> Int -> [Int]
altera list x y = 
    case list of {
        [] -> [],
        z :: rest -> 
            if z < x 
            then y :: (altera rest x y)
            else z :: (altera rest x y)
    }

main : [Int]
main = altera ([10,0,23,4,14,2,11]) 10 5
-- result = [10,5,23,5,14,5,11]