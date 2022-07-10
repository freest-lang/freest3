-- I exercise 1a

data Pair = Pair Int Int

-- without pattern matching
firstElement : Pair -> Int
firstElement x =
    case x of {
        Pair x _ -> x
    }

-- with pattern matching
-- firstElementPM : (Int,Int) -> Int
-- firstElementPM (x,_) = x

main : Int
main = firstElement (Pair 1 2)
--main = firstElementPM (1,2)

-- result = 1
