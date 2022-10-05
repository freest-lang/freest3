-- V exercise 19

data IntList = Nil | List Int IntList
data Tuple = T Int Int

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> Tuple -> Tuple) -> IntList -> Tuple -> Tuple
foldr' f list acc = foldl' f (reverseIntList list Nil) acc

reverseIntList : IntList -> IntList -> IntList
reverseIntList Nil           acc = acc
reverseIntList (List x rest) acc = reverseIntList rest (List x acc)

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> Tuple -> Tuple) -> IntList -> Tuple -> Tuple
foldl' f Nil           acc = acc
foldl' f (List x rest) acc = foldl' f rest (f x acc)

binary2decimal : IntList -> Int
binary2decimal list = fst' $ foldr' function list (T 0 0)

fst' : Tuple -> Int
fst' (T x _) = x

function : Int -> Tuple -> Tuple
function x (T acc i) = T (x*(pow 2 i)+acc) (i+1)

pow : Int -> Int -> Int
pow b e 
  | e <= 0    = 1 
  | otherwise = b * (pow b (e-1))

binary : IntList
binary = (List 1 (List 1 (List 0 (List 1 Nil))))

main : Int
main = binary2decimal binary
--result = 13
