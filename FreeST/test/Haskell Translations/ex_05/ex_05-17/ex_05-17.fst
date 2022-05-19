-- V exercise 17

data IntList = End | List Int IntList

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> Int -> Int) -> IntList -> Int -> Int
foldr' f list acc = foldl' f (reverseIntList list End) acc

reverseIntList : IntList -> IntList -> IntList
reverseIntList list acc =
    case list of {
        End -> acc,
        List x rest ->  reverseIntList rest (List x acc)
    }

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> Int -> Int) -> IntList -> Int -> Int
foldl' f list acc = 
    case list of {
        End -> acc,
        List x rest -> foldl' f rest (f x acc)
    }

sum' : IntList -> Int
sum' list = foldl' functionSum list 0

functionSum : Int -> Int -> Int
functionSum x acc = x + acc

length' : IntList -> Int
length' list = foldr' functionLength list 0

functionLength : Int -> Int -> Int
functionLength _ acc = 1 + acc

list1 : IntList
list1 = List (3) (List (2) (List (-1) End))

list2 : IntList
list2 = List 1 (List 2 (List 3 (List 4 End)))

main : Bool
main = (sum' list1) == (length' list2)
--result = True
