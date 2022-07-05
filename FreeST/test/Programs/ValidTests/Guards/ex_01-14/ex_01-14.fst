-- I exercise 14

data IntList = End | List Int IntList

replicate' : Int -> IntList
replicate' x = replicate'' x x

replicate'' : Int -> Int -> IntList
replicate'' x n 
    | n <= 0    = End 
    | otherwise = List x (replicate'' x (n-1))

reproduz : IntList -> IntList
reproduz End           = End
reproduz (List x rest) = concat' (replicate' x) (reproduz rest)

concat' : IntList -> IntList -> IntList
concat' End           list2 = list2
concat' (List x rest) list2 = List x (concat' rest list2)
    
main : IntList
main = reproduz (List 3 (List 5 (List 1 (List 0 End))))
-- result = List 3 (List 3 (List 3 (List 5 (List 5 (List 5 (List 5 (List 5 (List 1 End))))))))