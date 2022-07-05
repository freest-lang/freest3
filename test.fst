

data T = A T | B | C

f : T -> Int
f t = 
  case t of {
    A x -> 0,
    (A B) -> 1,
    y -> 2
  }

main : Int
main = f B