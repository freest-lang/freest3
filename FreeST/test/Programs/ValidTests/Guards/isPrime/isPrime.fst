{- |
Module      :  isPrime
Description :  This program checks if a numer is prime based on wilson's theorem
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

fact : Int -> Int
fact n | n == 0 = 1 | otherwise = n * fact (n - 1)

isPrime : Int -> Bool
isPrime x = rem (fact (x-1)) x == x-1

main : Bool
main = isPrime 17
