-- V exercise 21

data IntList = Nil | List Int IntList
data Tuple = T Int Int

foldr' : (Int -> Tuple -> Tuple) -> IntList -> Tuple -> Tuple
foldr' f list acc = foldl' f (reverseIntList list Nil) acc

reverseIntList : IntList -> IntList -> IntList
reverseIntList Nil           acc = acc
reverseIntList (List x rest) acc = reverseIntList rest (List x acc)

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> Tuple -> Tuple) -> IntList -> Tuple -> Tuple
foldl' f Nil           acc = acc
foldl' f (List x rest) acc = foldl' f rest (f x acc)

poly : Int -> IntList -> Int
poly x list = fst' $ foldr' (function x) list (T 0 0) 

fst' : Tuple -> Int
fst' (T x _) = x

function : Int -> Int -> Tuple -> Tuple
function x n (T acc i) = T ((n*(pow x i)) + acc) (i+1)

pow : Int -> Int -> Int
pow b e 
    | e <= 0    = 1 
    | otherwise = b * (pow b (e-1))

list : IntList
list = List 5 (List 2 (List 0 (List 1 (List 2 Nil))))

main : Int
main = poly 2 list
--result = 100
