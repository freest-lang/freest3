{- |
Module      :  MultipleOps
Description :  This program tests multiple operations and 
               the the result is the sum of them
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

main : Int
main = a + b + c + d

a : Int
a = -5 + 8 * 6

b : Int
b = rem (55+9) 9

c : Int
c = 20 + (div (-3 * 5) 5)

d : Int
d = 5 + (div 15 3) * 2 - (rem 8 3)

