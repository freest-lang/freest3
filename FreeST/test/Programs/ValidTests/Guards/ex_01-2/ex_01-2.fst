-- I exercise 2

absolute : Int -> Int
absolute x 
  | x < 0     = (-x)
  | otherwise = x

distance : Int -> Int -> Int -> Bool
distance a b c = (absolute (b-a)) < (absolute (c-a))

main : Bool
main = distance 1 2 3
-- result = True