-- I exercise 11a

getFacts : Int -> [Int]
getFacts x = getFacts' x (div x 2)

getFacts' : Int -> Int -> [Int]
getFacts' x d = if d <= 0 then [] else
                (if (mod x d) == 0
                    then d :: (getFacts' x (d-1))
                    else (getFacts' x (d-1))
                )

main : [Int]
main = getFacts 10000
-- result = [5000,2500,2000,1250,1000,625,500,400,250,200,125,100,80,50,40,25,20,16,10,8,5,4,2,1]