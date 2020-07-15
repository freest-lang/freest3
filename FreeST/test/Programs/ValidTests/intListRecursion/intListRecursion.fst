
mkList : Int -> [Int]
mkList x = if x == 0 then [] else x :: mkList (x-1)

getMultiples : Int -> [Int] -> [Int]
getMultiples n list =
    case list of {
        [] -> [],
        (x::xs) -> 
            if mod x n == 0 
                then x :: (getMultiples n xs)
                else getMultiples n xs
    }

main : [Int]
main = getMultiples 2 (getMultiples 9 (mkList 1000))