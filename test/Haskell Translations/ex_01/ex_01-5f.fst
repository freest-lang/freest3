-- I exercise 5f

data IntList = End | List Int IntList
data MaybeInt = Empty | Number Int

getElementNumber : Int -> IntList -> MaybeInt
getElementNumber i list = 
    case list of {
        End -> Empty,
        List x rest ->  if i <= 0
                            then Number x
                            else getElementNumber (i-1) rest
    }

main : MaybeInt
main = getElementNumber 3 (List 1 (List 2 (List 3 (List 4 (List 5 End)))))
-- result = 4