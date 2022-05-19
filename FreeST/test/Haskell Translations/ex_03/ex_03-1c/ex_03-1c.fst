-- I exercise 1c

data Triple = Triple Int Int Int

-- without pattern matching
firstElement : Triple -> Int
firstElement t = 
    case t of {
        Triple x _ _ -> x
    }

-- with pattern matching
--firstElementPM : (Int,Int,Int) -> Int
--firstElementPM (x,_,_) = x

main : Int
main = firstElement (Triple 1 2 3)
-- main = firstElementPM (1,2,3)

-- result = 1
