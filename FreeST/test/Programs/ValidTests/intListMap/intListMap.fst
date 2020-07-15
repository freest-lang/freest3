
mkList : Int -> [Int]
mkList x = if x == 0 then [] else x :: mkList (x-1)

map' : (Int -> Int) -> [Int] -> [Int]
map' f list =
    case list of {
        [] -> [],
        (x::xs) -> f x :: (map' f xs)
    }

nextPair : Int -> Int
nextPair x = if mod x 2 == 0 then x else x + 1

removeDups : [Int] -> [Int]
removeDups list = 
    case list of {
        [] -> [],
        (n :: rest) -> n :: (removeDups (removeDup n rest))
    }

removeDup : Int -> [Int] -> [Int]
removeDup n list =
    case list of {
        [] -> [],
        (number :: rest_of_list) ->
            if number == n 
                then removeDup n rest_of_list
                else number :: (removeDup n rest_of_list)
    }

main : [Int]
main = removeDups (map' nextPair (mkList 15))