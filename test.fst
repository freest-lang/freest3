
f : &{A: &{C: Skip, D: Skip}, B: Skip} 1-> Int
f c =
  case collect c of {
    (A (C c)) -> 0,
    (A (D c)) -> 1,
    (B c)     -> 2
  }

data T = A | B


main : Int
main = 1