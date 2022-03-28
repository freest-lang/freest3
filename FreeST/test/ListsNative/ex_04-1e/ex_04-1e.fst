-- IV exercise 1e

substitui : Int -> Int -> [Int] -> [Int]
substitui x y list = 
    case list of {
        [] -> [],
        z :: rest -> 
            if x == z 
            then y :: (substitui x y rest) 
            else z :: (substitui x y rest)
    }

main : [Int]
main = substitui 2 3 [1,2,3]
-- result = [1,3,3]