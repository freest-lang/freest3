-- I exercise 5d

data IntList = End | List Int IntList
data MaybeInt = Empty | Number Int

getSecond : IntList -> MaybeInt
getSecond list = 
    case list of {
        End -> Empty,
        List _ rest ->  case rest of {
                            End -> Empty,
                            List x _ -> Number x
                        }
    }

main : MaybeInt
main = getSecond (List 1 (List 2 (List 3 End)))
-- result = 2
