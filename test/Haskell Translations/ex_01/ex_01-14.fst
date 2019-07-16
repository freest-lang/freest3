-- I exercise 14

data IntList = End | List Int IntList

replicate' : Int -> IntList
replicate' x = replicate'' x x

replicate'' : Int -> Int -> IntList
replicate'' x n = if n <= 0 then End else List x (replicate'' x (n-1))

reproduz : IntList -> IntList
reproduz list = 
    case list of {
        End -> End,
        List x rest -> concat' (replicate' x) (reproduz rest)
    }

concat' : IntList -> IntList -> IntList
concat' list1 list2 = 
    case list1 of {
        End -> list2,
        List x rest -> List x (concat' rest list2)
    }
    
main : IntList
main = reproduz (List 3 (List 5 (List 1 (List 0 End))))
-- result = List 3 (List 3 (List 3 (List 5 (List 5 (List 5 (List 5 (List 5 (List 1 End))))))))