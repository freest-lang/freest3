-- I exercise 5e

data MaybeInt = Empty | Number Int

getPenultimate : [Int] -> MaybeInt
getPenultimate list = 
    case list of {
        [] -> Empty,
        x :: rest ->  
            case rest of {
                [] -> Empty,
                _ :: more ->  
                    case more of {
                        []     -> Number x,
                        _ :: _ -> getPenultimate rest
                    }
            }
    }

main : MaybeInt
main = getPenultimate ([1,2,3,4,5])
-- result Number 4