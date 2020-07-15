
mkList : Int -> [Int]
mkList x = if x == 0 then [] else x :: mkList (x-1)

reverse' : [Int] -> [Int]
reverse' list = reverse'' list []

reverse'' : [Int] -> [Int] -> [Int]
reverse'' list1 list2 =
    case list1 of {
        [] -> list2,
        (x::xs) -> reverse'' xs (x::list2)
    }

main : [Int]
main = reverse' (mkList 100)