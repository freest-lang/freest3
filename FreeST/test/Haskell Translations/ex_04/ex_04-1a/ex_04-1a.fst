-- IV exercise 1a

data IntList = End | List Int IntList

sum' : IntList -> Int
sum' list = 
    case list of {
        End -> 0,
        List x rest -> x + (sum' rest)
    }

main : Int
main = sum' (List 1 (List 2 (List 3 End)))
-- result = 6
