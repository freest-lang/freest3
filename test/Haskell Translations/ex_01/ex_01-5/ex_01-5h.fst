-- I exercise 5h

data IntList = End | List Int IntList

sum5First : IntList -> Int
sum5First list = sumNFirst 5 list

sumNFirst : Int -> IntList -> Int
sumNFirst n list = 
    case list of {
        End -> 0,
        List x rest ->  if n <= 0 
                            then 0
                            else x + sumNFirst (n-1) rest
    } 

main : Int
main = sum5First (List 1 (List 2 (List 3 (List 4 (List 5 (List 6 (List 7 End)))))))
-- result = 15