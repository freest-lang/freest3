-- V exercise 18

data IntList = End | List Int IntList

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldr' f list acc = foldl' f (reverseIntList list End) acc

reverseIntList : IntList -> IntList -> IntList
reverseIntList list acc =
    case list of {
        End -> acc,
        List x rest ->  reverseIntList rest (List x acc)
    }

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldl' f list acc = 
    case list of {
        End -> acc,
        List x rest -> foldl' f rest (f x acc)
    }

map' : (Int -> Int) -> IntList -> IntList
map' f list = foldr' (mapper f) list End

mapper : (Int -> Int) -> Int -> IntList -> IntList
mapper f x acc = List (f x) acc

filter' : (Int -> Bool) -> IntList -> IntList
filter' f list = foldr' (filtering f) list End

filtering : (Int -> Bool) -> Int -> IntList -> IntList
filtering f x acc = if f x then List x acc else acc

isPositive : Int -> Bool
isPositive x = x > 0

squareIt : Int -> Int
squareIt x = x*x

list : IntList
list = List (-2) (List (-1) (List 0 (List 1 (List 2 End))))

main : IntList
main = map' squareIt (filter' isPositive list)
--result = List 1 (List 4 End)