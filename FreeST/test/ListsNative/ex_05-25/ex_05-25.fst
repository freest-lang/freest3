-- V exercise 25

after : ([[Int]] -> [Int]) ->  ([Int] -> [[Int]]) -> ([Int] -> [Int]) 
after f1 f2 = (\list : [Int] -> f1 (f2 list))

concat' : [[Int]] -> [Int]
concat' list =
    case list of {
        [] -> [],
        x :: rest -> merge x (concat' rest)
    }

merge : [Int] -> [Int] -> [Int]
merge a b =
    case a of {
        [] -> b,
        x :: rest -> x :: (merge rest b)
    }

map' : (Int -> [Int]) -> [Int] -> [[Int]]
map' f list =
    case list of {
        [] -> [],
        x :: rest -> (f x) :: (map' f rest)
    }

{-            This does not work
filter' : (Int -> Bool) -> [Int] -> [Int]
filter' f l = 
    let box = (\x:Int -> if f x then [x] else []) in
    after concat' (map' box) l
-}

--              This does work
filter' : (Int -> Bool) -> [Int] -> [Int]
filter' f l = 
    after concat' (map' (\x:Int -> if f x then [x] else [])) l

list : [Int]
list = [-2,-1,-0,1,2]

main : [Int]
main = filter' (\x : Int -> x > 0) list
--result = [1,2]