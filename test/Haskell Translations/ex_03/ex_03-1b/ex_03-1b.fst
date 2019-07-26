-- I exercise 1b

data Pair = Pair Int Int

-- without pattern matching
switchElements : Pair -> Pair
switchElements x =
    case x of {
        Pair x y -> Pair y x
    }

-- with pattern matching
-- switchElementsPM : (Int,Int) -> (Int,Int)
-- switchElementsPM (x,y) = (y,x)

main : Pair
main = switchElements (Pair 1 2)
--main = switchElementsPM (1,2)

-- result = Pair 2 1