{- |
Module      :  Max
Description :  Returns the max value of two integers
Copyright   :  (c) LaSIGE and University of Lisbon, Portugal
Maintainer  :  balmeida@lasige.di.fc.ul.pt
-}


start :: Int
start = max' 17 23

max' :: Int -> Int -> Int
max' x y = if x >= y then x else y
