data IntList = Nil | Cons Int IntList

length' : IntList -> Int
length' l =
  case l of {
    Nil -> 0,
    Cons x y -> 1 + length' y
  }

main : Int
main = length' (Cons 5 (Cons 7 (Cons 23 (Cons 4 Nil))))

