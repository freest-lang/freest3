-- I exercise 8

multiplyBy2 : [Int] -> [Int]
multiplyBy2 list = 
    case list of {
        [] -> [],
        x :: rest -> (2*x) :: (multiplyBy2 rest)
    }

main : [Int]
main = multiplyBy2 [1,2,3]
-- result 2 :: 4 :: 6 :: []