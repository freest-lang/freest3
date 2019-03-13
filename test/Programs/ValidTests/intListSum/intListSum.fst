data IntList = Nil | Cons Int IntList

sum' : IntList -> Int
sum' l =
  case l of
    Nil -> 0;
    Cons x y -> x + sum' y


start : Int
start = sum' (Cons 5 (Cons 7 (Cons 2 (Cons 6 (Cons 3 Nil)))))

