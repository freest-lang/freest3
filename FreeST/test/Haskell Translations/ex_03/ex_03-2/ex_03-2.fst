-- I exercise 2

data Pair = Pair Int Int

-- without pattern matching
somaVec : Pair -> Pair -> Pair
somaVec u v =
    case u of {
        Pair x1 y1 -> case v of {
                        Pair x2 y2 -> Pair (x1+x2) (y1+y2)
                    }
    }

-- with pattern matching
-- somaVecPM : (Int,Int) -> (Int,Int) -> (Int,Int)
-- somaVecPM (x1,y1) (x2,y2) = (x1+x2,y1+y2)

main : Pair
main = somaVec (Pair 1 2) (Pair 3 4)
--main = somaVecPM (1,2)

-- result = Pair 4 6
