f : &{A: &{C: Skip, D: Skip}, B: Skip} 1-> Int
f c =
  case collect c of {
    (A (C c)) -> 0,
    (A (D c)) -> 1,
    (A c)     -> 2,
    c         -> 3
  }