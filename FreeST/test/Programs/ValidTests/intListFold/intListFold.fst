
mkList : Int -> [Int]
mkList x = if x == 0 then [] else x :: mkList (x-1)

foldl' : (Int -> Int -> Int) -> Int -> [Int] -> Int
foldl' f acc list =
    case list of {
        [] -> acc,
        (x::xs) ->
            foldl' f (f acc x) xs
    }

foldr' : (Int -> Int -> Int) -> Int -> [Int] -> Int
foldr' f acc list = foldl' (invertF f) acc (reverse' list)

invertF : (Int -> Int -> Int) -> (Int -> Int -> Int)
invertF f x y = f y x

reverse' : [Int] -> [Int]
reverse' list = reverse'' list []

reverse'' : [Int] -> [Int] -> [Int]
reverse'' list1 list2 =
    case list1 of {
        [] -> list2,
        (x::xs) ->
            reverse'' xs (x::list2)
    }

main : Bool
main = foldl' (+) 0 (mkList 1000) == foldr' (+) 0 (mkList 1000)