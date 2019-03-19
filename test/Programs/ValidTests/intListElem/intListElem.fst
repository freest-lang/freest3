data IntList = Nil | Cons Int IntList

elem' : Int -> IntList -> Bool
elem' a l =
  case l of {
    Nil -> False;
    Cons x y -> if a == x then True else elem' a y
  }

main : Bool
main = elem' 23 (Cons 5 (Cons 7 Nil))

