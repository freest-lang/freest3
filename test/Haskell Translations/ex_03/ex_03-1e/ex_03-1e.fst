-- I exercise 1e

data IntList = End | List Int IntList
data MaybeInt = Empty | Number Int

-- without pattern matching
secondElement : IntList -> MaybeInt
secondElement t = 
    case t of {
        End -> Empty,
        List _ rest -> case rest of {
                            End -> Empty,
                            List x _ -> Number x
                        }
    }

-- with pattern matching
-- secondElementPM : [a] -> a
-- secondElementPM (_:(x:_)) = x

main : MaybeInt
main = secondElement (List 1 (List 2 (List 3 End)))
-- main = secondElementPM (1:2:3:[])

-- result = Number 2