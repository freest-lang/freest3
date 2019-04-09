data List = Cons Int List | Nil

main : Bool
main = moreThanOneElem (Cons 2 (Cons 2 Nil))

moreThanOneElem : List -> Bool
moreThanOneElem l =
  case l of {
    Nil -> False;
    Cons i k z -> True
  }
