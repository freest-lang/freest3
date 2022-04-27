-- I exercise 1f

-- without pattern matching
secondElementFromHead : [(Int,Int)] -> Int
secondElementFromHead t = 
    case t of {
        pair :: _ -> 
            let (_,y) = pair in y
    }

-- with pattern matching
-- secondElementFromHeadPM : [(Int,Int)] -> Int
-- secondElementFromHeadPM ((_,x):_) = x

main : MaybeInt
main = secondElementFromHead ([(1,2),(3,4)])
-- result = 2