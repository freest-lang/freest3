data IntList = Nil | Cons Int IntList

null' :: IntList -> Bool
null' l =
  match l with
    Nil -> True
    Cons x y -> False


start :: Bool
start = null' (Cons 5 (Cons 7 Nil))

