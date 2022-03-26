-- I exercise 14

replicate' : Int -> [Int]
replicate' x = replicate'' x x

replicate'' : Int -> Int -> [Int]
replicate'' x n = if n <= 0 then [] else x :: (replicate'' x (n-1))

reproduz : [Int] -> [Int]
reproduz list = 
    case list of {
        [] -> [],
        x :: rest -> concat' (replicate' x) (reproduz rest)
    }

concat' : [Int] -> [Int] -> [Int]
concat' list1 list2 = 
    case list1 of {
        [] -> list2,
        x :: rest -> x :: (concat' rest list2)
    }
    
main : [Int]
main = reproduz [3,5,1,0]
-- result = 3 :: 3 :: 3 :: 5 :: 5 :: 5 :: 5 :: 5 :: 1 :: []