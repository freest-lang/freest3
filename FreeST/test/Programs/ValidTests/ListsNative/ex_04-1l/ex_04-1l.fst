-- IV exercise 1l

trocaPares : [Int] -> [Int]
trocaPares list = 
    case list of {
        [] -> [],
        x :: rest1 -> 
            case rest1 of {
                []         -> x :: rest1,
                y :: rest2 -> y :: x :: (trocaPares rest2)
            }
    }

main : [Int]
main = trocaPares ([1,2,3,4,5])
-- result = [2,1,4,3,5]