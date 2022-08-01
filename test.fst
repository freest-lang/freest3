
f : &{A: &{C: Skip, D: Skip}, B: Skip} 1-> Int

f c =
  case collect c of {
    (A (C c)) -> 0,
    (A (D c)) -> 1,
    (B c)     -> 2
  }


g : &{A: &{C: Skip, D: Skip}, B: Skip} 
 -> &{A: &{C: Skip, D: Skip}, B: Skip} 1-> Int

g (A (C c)) (B c2) = 0
g (A (C c)) (B c2) = 1
-- g (B c)     = 2

main : Int
main = 1