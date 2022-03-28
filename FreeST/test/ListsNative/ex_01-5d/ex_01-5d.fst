-- I exercise 5d

data MaybeInt = Empty | Number Int

getSecond : [Int] -> MaybeInt
getSecond list = 
    case list of {
        [] -> Empty,
        _ :: rest ->  
            case rest of {
                []     -> Empty,
                x :: _ -> Number x
            }
    }

main : MaybeInt
main = getSecond ([1,2,3])
-- result = Number 2