data IntList = Nil | Cons Int IntList

null' :: IntList -> Bool
null' l =
  case l of
    Nil -> True
    Cons x y -> False


start :: Bool
start = null' (Cons 5 (Cons 7 Nil))


