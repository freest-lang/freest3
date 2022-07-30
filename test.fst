
f : &{A: &{C: Skip, D: Skip}, B: Skip} 1-> Int
f c =
  case c of {
    (A (C c)) -> 0,
    (A (D c)) -> 1,
    (A c)     -> 2,
    c         -> 3
  }

g : &{A: &{C: Skip, D: Skip}, B: Skip} 1-> Int
g (A (C c)) = 0
g (A (D c)) = 1
g (A c)     = 2
g c         = 3

main : Int
main = 1