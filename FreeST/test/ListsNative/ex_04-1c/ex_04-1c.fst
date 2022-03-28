-- IV exercise 1c

data MaybeInt = Empty | Number Int

maximo : [Int] -> MaybeInt
maximo list = 
    case list of {
        []        -> Empty,
        x :: rest -> maximo' x rest
    }

maximo' : Int -> [Int] -> MaybeInt
maximo' max list = 
    case list of {
        [] -> Number max,
        x :: rest -> 
            if max > x
                then maximo' max rest
                else maximo' x rest
    }

main : MaybeInt
main = maximo [1,2,5,3,4]
-- result = 5