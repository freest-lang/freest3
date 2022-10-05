-- IV exercise 1b

data CharList = Nil | List Char CharList

replicate' : Int -> Char -> CharList
replicate' r c 
  | r == 0    = Nil 
  | otherwise = List c (replicate' (r-1) c)

main : CharList
main = replicate' 5 'c'
-- result = List 'c' (List 'c' (List 'c' (List 'c' (List 'c' Nil))))
