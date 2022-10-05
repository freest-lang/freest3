-- I exercise 1b

sum3_positives : Int -> Int -> Int -> Int
sum3_positives a b c 
  | (((a > 0) 
  &&  (b > 0)) 
  &&  (c > 0)) = a+b+c 
  | otherwise  = 0

main : Int
main = sum3_positives 1 2 3 + sum3_positives (-1) (-1) (-1)
-- retult = 6 + 0 = 6