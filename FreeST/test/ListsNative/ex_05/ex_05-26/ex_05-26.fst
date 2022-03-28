-- V exercise 26

--foldr' : (x -> acc -> acc) -> xs -> acc -> acc 
foldr' : (Int -> (Int,Int) -> (Int,Int)) -> [Int] -> (Int,Int) -> (Int,Int)
foldr' f list acc = foldl' f (reverseIntList list []) acc

reverseIntList : [Int] -> [Int] -> [Int]
reverseIntList list acc =
    case list of {
        [] -> acc,
        x :: rest -> reverseIntList rest (x :: acc)
    }

--foldl' : (x -> acc -> acc) -> xs -> acc -> acc 
foldl' : (Int -> (Int,Int) -> (Int,Int)) -> [Int] -> (Int,Int) -> (Int,Int)
foldl' f list acc = 
    case list of {
        [] -> acc,
        x :: rest -> foldl' f rest (f x acc)
    }

sumlen : [Int] -> (Int,Int)
sumlen list = foldr' sumlen' list (0,0)

sumlen' : Int -> (Int,Int) -> (Int,Int)
sumlen' x tuple =
    let (a,b) = tuple in ((a+x),(b+1))

list : [Int]
list = [1,2,3,4]

main : (Int,Int)
main = sumlen list
--result = (10,4)