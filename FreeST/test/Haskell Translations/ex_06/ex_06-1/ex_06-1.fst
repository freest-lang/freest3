-- VI exercise 1

data Set = S IntList
data IntList = End | List Int IntList

-- functions should mantain Set's properties:
--      ordered, no element repetition

empty : Set
empty = S End

isNull : Set -> Bool
isNull set =
    case set of {
        S list -> 
            case list of {
                End -> True,
                List _ _ -> False
            }
    }

singleton : Int ->  Set
singleton x = S (List x End)

member : Int -> Set -> Bool
member x set =
    case set of {
        S list -> 
            case list of {
                End -> False,
                List y rest -> 
                    if x == y 
                        then True
                        else member x (S rest)
            }
    }

fromList : IntList -> Set
fromList list = S list

insert : Int -> Set -> Set
insert x set =
    case set of {
        S list -> S (insertOrd x list)
    }

insertOrd : Int -> IntList -> IntList
insertOrd x list = 
    case list of {
        End -> List x End,
        List y rest -> 
            if x == y
                then list
                else if x < y
                        then List x list
                        else List y (insertOrd x rest) 
    }

filterSet : (Int -> Bool) -> Set -> Set
filterSet p set =
    case set of {
        S list -> S (filterOrd p list)
    }

filterOrd : (Int -> Bool) -> IntList -> IntList
filterOrd p list =
    case list of {
        End -> End,
        List x rest -> 
            if p x
                then List x (filterOrd p rest)
                else filterOrd p rest
    }

remove : Int -> Set -> Set
remove x set =
    case set of {
        S list -> S (removeFromList x list)
    }

removeFromList : Int -> IntList -> IntList
removeFromList x list =
    case list of {
        End -> End,
        List y rest ->
            if x == y
                then removeFromList x rest
                else List y (removeFromList x rest) 
    }

union : Set -> Set -> Set
union set1 set2 =
    case set1 of {
        S list1 -> case set2 of {
                        S list2 -> S (removeDup (unionList list1 list2))
                    }
    }

unionList : IntList -> IntList -> IntList
unionList list1 list2 =
    case list1 of {
        End -> list2,
        List x rest1 -> 
            case list2 of {
                End -> list1,
                List y rest2 ->
                    if x < y
                        then List x (unionList rest1 list2) 
                        else List y (unionList list1 rest2)
            }
    }

removeDup : IntList -> IntList
removeDup list =
    case list of {
        End -> End,
        List x rest1 ->
            case rest1 of {
                End -> list,
                List y rest2 -> 
                    if x == y
                        then removeDup rest1
                        else List x (removeDup rest1)
            }
    }

intersection : Set -> Set -> Set
intersection set1 set2 =
    case set1 of {
        S list1 -> case set2 of {
                        S list2 -> S (intersectionList list1 list2)
                    }
    }

intersectionList : IntList -> IntList -> IntList
intersectionList list1 list2 = 
    case list1 of {
        End -> End,
        List x rest1 ->
            case list2 of {
                End -> End,
                List y rest2 ->
                    if x == y 
                        then List x (intersectionList rest1 rest2) 
                        else if x < y 
                                then intersectionList rest1 list2
                                else intersectionList list1 rest2
            }
    }

difference : Set -> Set -> Set
difference set1 set2 =
    case set1 of {
        S list1 -> case set2 of {
                        S list2 -> S (differenceList list1 list2)
                    }
    }

differenceList : IntList -> IntList -> IntList
differenceList list1 list2 = 
    case list1 of {
        End -> list2,
        List x rest1 ->
            case list2 of {
                End -> list1,
                List y rest2 ->
                    if x == y 
                        then differenceList rest1 rest2
                        else if x < y 
                                then List x (differenceList rest1 list2)
                                else List y (differenceList list1 rest2)
            }
    }

size : Set -> Int
size set = 
    case set of {
        S list -> 
            case list of {
                End -> 0,
                List _ rest -> 1 + (size (S rest))
            }
    }

data Tuple = Tuple Set Set

partition : (Int -> Bool) -> Set -> Tuple
partition p set = Tuple (filterSet p set) (filterSet (\x : Int -> not (p x)) set)

isPair : Int -> Bool
isPair x = mod x 2 == 0

main : Tuple
main = 
    let set1 = insert 1 (insert 2 (insert 3 empty)) in
    let set2 = insert 2 (insert 3 (insert 4 empty)) in
    
    let _ = empty                           in -- S End
    let _ = isNull empty                    in -- True
    let _ = isNull set1                     in -- False
    let _ = singleton 1                     in -- S (List 1 End)
    let _ = member 1 set1                   in -- True
    let _ = member 2 set2                   in -- False
    let _ = insert 1 set1                   in -- S (List 1 (List 2 (List 3 End)))
    let _ = insert 5 set2                   in -- S (List 2 (List 3 (List 4 (List 5 End))))
    let list = List 1 (List 2 (List 3 End)) in
    let _ = fromList list                   in -- S (List 1 (List 2 (List 3 End)))
    -- let f = (\x : Int -> mod x 2 == 0)   in -- function, Pair = True, Odd = False
    let _ = filterSet isPair set1           in -- S (List 2 End)
    let _ = remove 1 set1                   in -- S (List 2 (List 3 End))
    let _ = remove 2 set1                   in -- S (List 1 (List 3 End))
    let _ = remove 3 set1                   in -- S (List 1 (List 2 End))
    let u = union set1 set2                 in -- S (List 1 (List 2 (List 3 (List 4 End))))
    let _ = intersection set1 set2          in -- S (List 2 (List 3 End))
    let _ = difference set1 set2            in -- S (List 1 (List 4 End))
    let _ = (size set1) == (size set2)      in -- True
    partition isPair u
-- result = Tuple (S (List 2 (List 4 End))) (S (List 1 (List 3 End)))
