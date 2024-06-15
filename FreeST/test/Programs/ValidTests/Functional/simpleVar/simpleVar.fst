{- |
Module      :  Test a simple variable
Description :  Returns the value of a var passed by parameter (id function)
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

id' : Int -> Int
id' x = x

main : Int
main = id' 23

