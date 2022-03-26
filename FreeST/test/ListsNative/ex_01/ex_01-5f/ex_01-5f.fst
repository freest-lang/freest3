-- I exercise 5f

data MaybeInt = Empty | Number Int

getElementNumber : Int -> [Int] -> MaybeInt
getElementNumber i list = 
    case list of {
        [] -> Empty,
        x :: rest ->  
            if i <= 0
            then Number x
            else getElementNumber (i-1) rest
    }

main : MaybeInt
main = getElementNumber [3,1,2,3,4,5]
-- result = Number 4