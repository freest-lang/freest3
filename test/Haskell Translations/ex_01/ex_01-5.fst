-- I exercicio 5

-- "Prelude" Functions

data IntList = End | List Int IntList
data MaybeInt = Empty | Number Int

addHead : Int -> IntList -> IntList
addHead head list = List head list

getHead : IntList -> MaybeInt
getHead list = 
    case list of {
        End -> Empty,
        List h _ -> Number h
    }

getTail : IntList -> IntList
getTail list =
    case list of {
        End -> End,
        List _ list -> list
    }

getLast : IntList -> MaybeInt
getLast list = 
    case list of {
        End -> Empty,
        List x rest ->  case rest of {
                            End -> Number x,
                            List _ _ -> getLast rest
                        }
    }

getInit : IntList -> IntList
getInit list = 
    case list of {
        End -> End,
        List x rest ->  case rest of {
                            End -> End,
                            List y _ -> List x (getInit rest)
                        }
    }

isNull : IntList -> Bool
isNull list = 
    case list of {
        End -> True,
        List _ _ -> False
    }

listLength : IntList -> Int
listLength list = 
    case list of {
        End -> 0,
        List _ rest -> 1 + listLength rest
    }

listReverse : IntList -> IntList
listReverse list = listShifter list End

listShifter : IntList -> IntList -> IntList
listShifter list invertedList = 
    case list of {
        End -> invertedList,
        List x rest -> listShifter rest (List x invertedList)
    }

listTake : Int -> IntList -> IntList
listTake t list = 
    case list of {
        End -> End,
        List x rest -> if t <= 0 then End else List x (listTake (t-1) rest)
    }

listSum : IntList -> Int
listSum list = 
    case list of {
        End -> 0,
        List x rest -> x + listSum rest
    }

main : Int
main =
    let x = List 1 (List 2 (List 3 End))    in -- result = (List 1 (List 2 (List 3 End))))
    let _ = getHead x                       in -- result = 1
    let _ = getTail x                       in -- result = (List 2 (List 3 End))
    let _ = getLast x                       in -- result = Number 3
    let _ = getInit x                       in -- result = (List 1 (List 2 End))
    let _ = isNull x                        in -- result = False
    let _ = listLength x                    in -- result = 3
    let _ = listReverse x                   in -- result = (List 3 (List 2 (List 1 End)))
    let _ = listTake 2 x                    in -- result = (List 1 (List 2 End))
            listSum x                          -- result = 6