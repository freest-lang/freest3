-- I exercise 1d

data Triple = Int Int Int

-- without pattern matching
firstElement : Triple -> Triple
firstElement t = 
    case t of {
        Triple x y z -> Triple y x z
    }

-- with pattern matching
-- firstElementPM : (Int,Int,Int) -> (Int,Int,Int)
-- firstElementPM (x,y,z) = (y,x,z)

main : Int
main = firstElement (Triple 1 2 3)
-- main = firstElementPM (1,2,3)

-- result = Triple 2 1 3