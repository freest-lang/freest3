-- I exercise 5e

data IntList = End | List Int IntList
data MaybeInt = Empty | Number Int

getPenultimate : IntList -> MaybeInt
getPenultimate list = 
    case list of {
        End -> Empty,
        List x rest ->  case rest of {
                            End -> Empty,
                            List _ more ->  case more of {
                                                End -> Number x,
                                                List _ _ -> getPenultimate rest
                                            }
                        }
    }

main : MaybeInt
main = getPenultimate (List 1 (List 2 (List 3 (List 4 (List 5 End)))))
-- result Number 4