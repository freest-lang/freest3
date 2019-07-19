-- I exercise 5i

data IntList = End | List Int IntList

sumNFirst : Int -> IntList -> Int
sumNFirst n list = 
    case list of {
        End -> 0,
        List x rest ->  if n <= 0 
                            then 0
                            else x + sumNFirst (n-1) rest
    } 

main : Int
main = sumNFirst 6 (List 1 (List 2 (List 3 (List 4 (List 5 (List 6 (List 7 End)))))))
-- result = 15