-- IV exercise 1b

data CharList = End | List Char CharList

replicate' : Int -> Char -> CharList
replicate' r c 
  | r == 0    = End 
  | otherwise = List c (replicate' (r-1) c)

main : CharList
main = replicate' 5 'c'
-- result = List 'c' (List 'c' (List 'c' (List 'c' (List 'c' End))))