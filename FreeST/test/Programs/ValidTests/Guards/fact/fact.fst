{- |
Module      :  Fact
Description :  This program calculates the factorial of 12
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

fact : Int -> Int
fact n | n == 0 = 1 | otherwise = n * fact (n - 1)

main : Int
main = fact 12
