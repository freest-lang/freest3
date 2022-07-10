-- I exercise 10

-- should be used Double/Long values instead of Int values

data Solution = S0 | S1 Int | S2 Int Int

data MaybeInt = Empty | Number Int

calculates : Int -> Int -> Int -> Solution
calculates a b c = 
    let delta = b*b-4*a*c in
    if delta < 0 
        then S0                     -- doesn't exist a valid square root
        else    let s = squareRoot delta in
                case s of {
                    Empty -> S0,    -- doesn't exist a valid square root
                    Number sqrt ->  let x1 = div ((-b)+sqrt) (2*a)   in
                                    let x2 = div ((-b)-sqrt) (2*a)   in
                                    if delta == 0 
                                        then S1 x1 
                                        else S2 x1 x2
                }

squareRoot : Int -> MaybeInt
squareRoot x = squareRoot' x 1
squareRoot' : Int -> Int -> MaybeInt
squareRoot' x i =    if i > x 
                        then Empty 
                        else if i*i == x 
                                then Number i 
                                else squareRoot' x (i+1)

main : Solution
main = calculates 2 2 (-4)
-- result = S2 1 (-2)
