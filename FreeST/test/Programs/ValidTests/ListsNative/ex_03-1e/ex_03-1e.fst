-- I exercise 1e

-- without pattern matching
secondElement : [Int] -> Int
secondElement t = 
    case t of {
        _ :: rest -> 
            case rest of {
                x :: _ -> x
            }
    }

-- with pattern matching
-- secondElementPM : [a] -> a
-- secondElementPM (_:(x:_)) = x

main : Int
main = secondElement ([1,2,3])
-- main = secondElementPM (1::2::3::[])

-- result = 2