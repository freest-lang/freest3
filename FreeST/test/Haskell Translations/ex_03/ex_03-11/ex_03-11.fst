-- I exercise 11

-- should be used Double/Long values instead of Int values

data IntList = End | List Int IntList

data MaybeInt = Empty | Number Int

calculates : Int -> Int -> Int -> IntList
calculates a b c = 
    let delta = b*b-4*a*c in
    if delta < 0 
        then End                    -- doesn't exist a valid square root
        else    let s = squareRoot delta in
                case s of {
                    Empty -> End,   -- doesn't exist a valid square root
                    Number sqrt ->  let x1 = div ((-b)+sqrt) (2*a)   in
                                    let x2 = div ((-b)-sqrt) (2*a)   in
                                    if delta == 0 
                                        then List x1 End
                                        else List x1 (List x2 End)
                }

squareRoot : Int -> MaybeInt
squareRoot x = squareRoot' x 1
squareRoot' : Int -> Int -> MaybeInt
squareRoot' x i =    if i > x 
                        then Empty 
                        else if i*i == x 
                                then Number i 
                                else squareRoot' x (i+1)

main : IntList
main = calculates 2 2 (-4)
-- result = List 1 (List (-2) End)