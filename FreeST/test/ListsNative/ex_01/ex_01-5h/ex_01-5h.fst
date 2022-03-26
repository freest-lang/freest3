-- I exercise 5h

sum5First : [Int] -> Int
sum5First list = sumNFirst 5 list

sumNFirst : Int -> [Int] -> Int
sumNFirst n list = 
    case list of {
        [] -> 0,
        x :: rest ->  
            if n <= 0 
            then 0
            else x + sumNFirst (n-1) rest
    } 

main : Int
main = sum5First [1,2,3,4,5,6,7]
-- result = 15