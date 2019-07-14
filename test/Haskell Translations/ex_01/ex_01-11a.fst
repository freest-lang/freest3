-- I exercise 11a

data IntList = End | List Int IntList

getFacts : Int -> IntList
getFacts x = getFacts' x (div x 2)
getFacts' : Int -> Int -> IntList
getFacts' x d = if d <= 0 then End else
                (if (mod x d) == 0
                    then List d (getFacts' x (d-1))
                    else (getFacts' x (d-1))
                )

main : IntList
main = getFacts 10000