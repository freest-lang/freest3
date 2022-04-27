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

writePrimes : [Int] -> [(Int,Int)]
writePrimes list =
    case list of {
        []  -> [],
        x :: rest -> (x,(nPrime x)) :: (writePrimes rest)
    }

main : ListTuple
main = writePrimes ([7,78,1453,0])
--result = [(7,19),(78,401),(1453,12149),(0,2)]