-- V exercise 17

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> Int -> Int) -> [Int] -> Int -> Int
foldr' f list acc = foldl' f (reverseIntList list []) acc

reverseIntList : [Int] -> [Int] -> [Int]
reverseIntList list acc =
    case list of {
        [] -> acc,
        x :: rest ->  reverseIntList rest (x :: acc)
    }

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> Int -> Int) -> [Int] -> Int -> Int
foldl' f list acc = 
    case list of {
        [] -> acc,
        x :: rest -> foldl' f rest (f x acc)
    }

sum' : [Int] -> Int
sum' list = foldl' functionSum list 0

functionSum : Int -> Int -> Int
functionSum x acc = x + acc

length' : [Int] -> Int
length' list = foldr' functionLength list 0

functionLength : Int -> Int -> Int
functionLength _ acc = 1 + acc

list1 : [Int]
list1 = [3,2,-1]

list2 : [Int]
list2 = [1,2,3,4]

main : Bool
main = (sum' list1) == (length' list2)
--result = True