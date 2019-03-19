{- |
Module      :  Min
Description :  Returns the max value of two integers
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}

main : Int
main = min' 17 23

min' : Int -> Int -> Int
min' x y = if x < y then x else y
