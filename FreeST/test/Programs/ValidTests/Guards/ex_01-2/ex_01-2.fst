-- I exercise 2

distance : Int -> Int -> Int -> Bool
distance a b c = (absolute (b-a)) < (absolute (c-a))

absolute : Int -> Int
absolute x 
  | x < 0     = (-x)
  | otherwise = x

main : Bool
main = distance 1 2 3
-- result = True