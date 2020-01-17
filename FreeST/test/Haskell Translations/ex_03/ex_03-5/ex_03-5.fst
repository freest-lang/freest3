-- I exercise 5

data Pair = Pair Int Int

quadrant : Pair -> Int
quadrant pair = 
    case pair of {
        Pair x y -> if (x > 0) && (y > 0) then 1 else
                    if (x < 0) && (y > 0) then 2 else
                    if (x < 0) && (y < 0) then 3 else
                    if (x > 0) && (y > 0) then 4 else
                    0
    }

main : Int
main = quadrant (Pair (-10) 10)
-- result = 2