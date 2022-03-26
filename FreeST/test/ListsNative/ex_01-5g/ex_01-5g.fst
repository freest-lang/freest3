-- I exercise 5g

invertAllButFirst : [Int] -> [Int]
invertAllButFirst list = 
    case list of {
        [] -> [],
        x :: rest -> x :: (invert rest)
    }

invert : [Int] -> [Int] 
invert list = invert' list []

invert' : [Int] -> [Int] -> [Int]
invert' from to = 
    case from of {
        []        -> to,
        x :: rest -> invert' rest (x :: to)
    }

main : [Int]
main = invertAllButFirst [1,2,3,4,5]
-- result = 1 :: 5 :: 4 :: 3 :: 2 :: []