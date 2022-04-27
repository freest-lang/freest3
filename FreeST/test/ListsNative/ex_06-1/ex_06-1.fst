-- VI exercise 1

data Set = S [Int]

-- functions should mantain Set's properties:
--      ordered, no element repetition

empty : Set
empty = S []

isNull : Set -> Bool
isNull set =
    case set of {
        S list -> 
            case list of {
                [] -> True,
                x :: xs -> False
            }
    }

singleton : Int ->  Set
singleton x = S ([x])

member : Int -> Set -> Bool
member x set =
    case set of {
        S list -> 
            case list of {
                [] -> False,
                y :: rest -> 
                    if x == y 
                        then True
                        else member x (S rest)
            }
    }

fromList : [Int] -> Set
fromList list = S list

insert : Int -> Set -> Set
insert x set =
    case set of {
        S list -> S (insertOrd x list)
    }

insertOrd : Int -> [Int] -> [Int]
insertOrd x list = 
    case list of {
        [] -> [x],
        y :: rest -> 
            if x == y
                then list
                else if x < y
                        then x :: list
                        else y :: (insertOrd x rest) 
    }

filterSet : (Int -> Bool) -> Set -> Set
filterSet p set =
    case set of {
        S list -> S (filterOrd p list)
    }

filterOrd : (Int -> Bool) -> [Int] -> [Int]
filterOrd p list =
    case list of {
        [] -> [],
        x :: rest -> 
            if p x
                then x :: (filterOrd p rest)
                else filterOrd p rest
    }

remove : Int -> Set -> Set
remove x set =
    case set of {
        S list -> S (removeFromList x list)
    }

removeFromList : Int -> [Int] -> [Int]
removeFromList x list =
    case list of {
        [] -> [],
        y :: rest ->
            if x == y
                then removeFromList x rest
                else y :: (removeFromList x rest) 
    }

union : Set -> Set -> Set
union set1 set2 =
    case set1 of {
        S list1 -> case set2 of {
                        S list2 -> S (removeDup (unionList list1 list2))
                    }
    }

unionList : [Int] -> [Int] -> [Int]
unionList list1 list2 =
    case list1 of {
        [] -> list2,
        x :: rest1 -> 
            case list2 of {
                [] -> list1,
                y :: rest2 ->
                    if x < y
                        then x :: (unionList rest1 list2) 
                        else y :: (unionList list1 rest2)
            }
    }

removeDup : [Int] -> [Int]
removeDup list =
    case list of {
        [] -> [],
        x :: rest1 ->
            case rest1 of {
                [] -> list,
                y :: rest2 -> 
                    if x == y
                        then removeDup rest1
                        else x :: (removeDup rest1)
            }
    }

intersection : Set -> Set -> Set
intersection set1 set2 =
    case set1 of {
        S list1 -> case set2 of {
                        S list2 -> S (intersectionList list1 list2)
                    }
    }

intersectionList : [Int] -> [Int] -> [Int]
intersectionList list1 list2 = 
    case list1 of {
        [] -> [],
        x :: rest1 ->
            case list2 of {
                [] -> [],
                y :: rest2 ->
                    if x == y 
                        then x :: (intersectionList rest1 rest2) 
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

differenceList : [Int] -> [Int] -> [Int]
differenceList list1 list2 = 
    case list1 of {
        [] -> list2,
        x :: rest1 ->
            case list2 of {
                [] -> list1,
                y :: rest2 ->
                    if x == y 
                        then differenceList rest1 rest2
                        else if x < y 
                                then x :: (differenceList rest1 list2)
                                else y :: (differenceList list1 rest2)
            }
    }

size : Set -> Int
size set = 
    case set of {
        S list -> 
            case list of {
                [] -> 0,
                _::rest -> 1 + (size (S rest))
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
    
    let _ = empty                           in -- S []
    let _ = isNull empty                    in -- True
    let _ = isNull set1                     in -- False
    let _ = singleton 1                     in -- S [1]
    let _ = member 1 set1                   in -- True
    let _ = member 2 set2                   in -- False
    let _ = insert 1 set1                   in -- S [1,2,3]
    let _ = insert 5 set2                   in -- S [2,3,4,5]
    let list = [1,2,3]                      in
    let _ = fromList list                   in -- S [1,2,3]
    -- let f = (\x : Int -> mod x 2 == 0)   in -- function, Pair = True, Odd = False
    let _ = filterSet isPair set1           in -- S [2]
    let _ = remove 1 set1                   in -- S [2,3,]
    let _ = remove 2 set1                   in -- S [1,3,]
    let _ = remove 3 set1                   in -- S [1,2]
    let u = union set1 set2                 in -- S [1,2,3,4]
    let _ = intersection set1 set2          in -- S [2,3]
    let _ = difference set1 set2            in -- S [1,4]
    let _ = (size set1) == (size set2)      in -- True
    partition isPair u
-- result = Tuple (S ([2,4])) (S ([1,3]))