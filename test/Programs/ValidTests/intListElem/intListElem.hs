data IntList = Nil | Cons Int IntList

elem' :: Int -> IntList -> Bool
elem' a l =
  case l of
    Nil -> False
    Cons x y -> if a == x then True else elem' a y


start :: Bool
start = elem' 23 (Cons 5 (Cons 7 Nil))

