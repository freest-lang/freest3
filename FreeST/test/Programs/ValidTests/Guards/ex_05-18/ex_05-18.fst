-- V exercise 18

data IntList = Nil | List Int IntList

reverseIntList : IntList -> IntList -> IntList
reverseIntList Nil           acc = acc
reverseIntList (List x rest) acc = reverseIntList rest (List x acc)

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldl' f Nil           acc = acc
foldl' f (List x rest) acc = foldl' f rest (f x acc)

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> IntList -> IntList) -> IntList -> IntList -> IntList
foldr' f list acc = foldl' f (reverseIntList list Nil) acc

mapper : (Int -> Int) -> Int -> IntList -> IntList
mapper f x acc = List (f x) acc

map' : (Int -> Int) -> IntList -> IntList
map' f list = foldr' (mapper f) list Nil

filtering : (Int -> Bool) -> Int -> IntList -> IntList
filtering f x acc 
    | f x       = List x acc
    | otherwise = acc

filter' : (Int -> Bool) -> IntList -> IntList
filter' f list = foldr' (filtering f) list Nil

isPositive : Int -> Bool
isPositive x = x > 0

squareIt : Int -> Int
squareIt x = x*x

list : IntList
list = List (-2) (List (-1) (List 0 (List 1 (List 2 Nil))))

main : IntList
main = map' squareIt (filter' isPositive list)
--result = List 1 (List 4 Nil)
