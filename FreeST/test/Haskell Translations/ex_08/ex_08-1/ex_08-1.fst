-- VIII exercise 1

isPrime : Int -> Bool
isPrime x = 
    if x <= 1 then False else isPrime' x (x-1)

isPrime' : Int -> Int -> Bool
isPrime' x y = 
    if y <= 1 then True else mod x y /= 0 && isPrime' x (y-1)

nPrime : Int -> Int
nPrime x = nPrime' x 2

nPrime' : Int -> Int -> Int
nPrime' n x = 
    if n <= 0 && isPrime x 
        then x 
        else
            if isPrime x 
                then nPrime' (n-1) (x+1)
                else nPrime' n (x+1)

data List = End | List Int List
data ListTuple = E | L Int Int ListTuple

writePrimes : List -> ListTuple
writePrimes list =
    case list of {
        End -> E,
        List x rest -> L x (nPrime x) (writePrimes rest)
    }

main : ListTuple
main = writePrimes (List 7 (List 78 (List 1453 (List 0 End))))
--result = L 7 19 (L 78 401 (L 1453 12149 (L 0 2 E)))
