-- I exercise 4

-- without pattern matching
subtract1 : Int -> Int
subtract1 x
  | x == 0   = 0 
  |otherwise = x - 1

-- with pattern matching
-- subtract1PM : Int -> Int
-- subtract1PM 0 = 0
-- subtract1PM x = x - 1

main : Int
main = subtract1 10
-- main = subtract1PM 10

-- result = 9