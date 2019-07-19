-- I exercise 1f

data Pair = Pair Int Int
data PairList = End | List Pair PairList
data MaybeInt = Empty | Number Int

-- without pattern matching
secondElementFromHead : PairList -> MaybeInt
secondElementFromHead t = 
    case t of {
        End -> Empty,
        List pair _ -> case pair of {
                                Pair _ y -> Number y 
                            }
    }

-- with pattern matching
-- secondElementFromHeadPM : [(Int,Int)] -> Int
-- secondElementFromHeadPM ((_,x):_) = x

main : MaybeInt
main = secondElementFromHead (List (Pair 1 2) (List (Pair 3 4) End))
-- main = secondElementFromHeadPM (1,2):(3,4):[]

-- result = Number 2