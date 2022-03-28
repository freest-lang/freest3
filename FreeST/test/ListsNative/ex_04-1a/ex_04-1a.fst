-- IV exercise 1a

sum' : [Int] -> Int
sum' list = 
    case list of {
        [] -> 0,
        x :: rest -> x + (sum' rest)
    }

main : Int
main = sum' $ [1,2,3]
-- result = 6