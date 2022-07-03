

data T = A | B

f : T -> Int -> Int -> Int
f x y z =
  case x of {
    A | y == z    -> 0
      | otherwise -> 1, 
    B | y == z    -> 2
      | otherwise -> 3
  }

main : Int
main = f B 1 2