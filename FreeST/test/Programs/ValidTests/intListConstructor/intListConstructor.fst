
constructList : Int -> [Int]
constructList x = if (x == 0) then [] else x :: (constructList (x-1))

main : [Int]
main = constructList 20