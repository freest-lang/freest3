-- VII exercise 11

data Set = End | Set Int Set

insertSet : Int -> Set -> Set
insertSet x set =
    case set of {
        End -> Set x End,
        Set y rest ->
            if x == y
                then set
                else 
                    if x < y
                        then Set x set
                        else Set y (insertSet x rest)
        }

fmap' : (Int -> Int) -> Set -> Set
fmap' f set = fmap'' f set End

fmap'' : (Int -> Int) -> Set -> Set -> Set
fmap'' f set1 set2 =
    case set1 of {
        End -> set2,
        Set x rest -> fmap'' f rest (insertSet (f x) set2)
    }

main : Set
main = 
    let set = insertSet 5 (insertSet 4 (insertSet 8 (insertSet 7 (insertSet 1 End))))   in
    let f = (\x:Int -> if x < 6 then x*x else div x 2)                                  in
    fmap' f set
--result = Set 1 (Set 3 (Set 4 (Set 16 (Set 25 End))))