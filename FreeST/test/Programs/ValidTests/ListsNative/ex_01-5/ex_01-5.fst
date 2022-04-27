-- I exercicio 5

-- "Prelude" Functions

data MaybeInt = Empty | Number Int

addHead : Int -> [Int] -> [Int]
addHead head list = head :: list

getHead : [Int] -> MaybeInt
getHead list = 
    case list of {
        []     -> Empty,
        h :: _ -> Number h
    }

getTail : [Int] -> [Int]
getTail list =
    case list of {
        []        -> [],
        _ :: list -> list
    }

getLast : [Int] -> MaybeInt
getLast list = 
    case list of {
        [] -> Empty,
        x :: rest ->  
            case rest of {
                []     -> Number x,
                _ :: _ -> getLast rest
            }
    }

getInit : [Int] -> [Int]
getInit list = 
    case list of {
        [] -> [],
        x :: rest ->  
            case rest of {
                []     -> [],
                y :: _ -> x :: (getInit rest)
            }
    }

isNull : [Int] -> Bool
isNull list = 
    case list of {
        []     -> True,
        _ :: _ -> False
    }

listLength : [Int] -> Int
listLength list = 
    case list of {
        [] -> 0,
        _ :: rest -> 1 + listLength rest
    }

listReverse : [Int] -> [Int]
listReverse list = listShifter list []

listShifter : [Int] -> [Int] -> [Int]
listShifter list invertedList = 
    case list of {
        []        -> invertedList,
        x :: rest -> listShifter rest (x :: invertedList)
    }

listTake : Int -> [Int] -> [Int]
listTake t list = 
    case list of {
        [] -> [],
        x :: rest -> 
            if t <= 0 
            then [] 
            else x :: (listTake (t-1) rest)
    }

listSum : [Int] -> Int
listSum list = 
    case list of {
        []        -> 0,
        x :: rest -> x + listSum rest
    }

main : Int
main =
    let x = [1,2,3]                         in -- result = 1 :: 2 :: 3 :: []
    let _ = getHead x                       in -- result = 1
    let _ = getTail x                       in -- result = 2 :: 3 :: []
    let _ = getLast x                       in -- result = Number 3
    let _ = getInit x                       in -- result = 1 :: 2 :: []
    let _ = isNull x                        in -- result = False
    let _ = listLength x                    in -- result = 3
    let _ = listReverse x                   in -- result = 3 :: 2 :: 1 :: []
    let _ = listTake 2 x                    in -- result = 1 :: 2 :: []
            listSum x                          -- result = 6