-- IV exercise 1d

elem' : Int -> [Int] -> Bool
elem' x list = 
    case list of {
        [] -> False,
        y :: rest -> 
            if x == y 
                then True 
                else elem' x rest
    }

main : Bool
main = elem' 3 [1,2,3,4]
-- result = True