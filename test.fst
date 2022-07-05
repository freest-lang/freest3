

data Pair = Pair Int Int
data Pairs = N | P Pair Pairs

f : T -> Int
f t = 
  case t of {
    A x -> 0,
    (A B) -> 1,
    y 
      | 1 == 2 -> 2
      | _ -> 2
  }

main : Int
main = f $ P (Pair 1 1) $ P (Pair 2 2) N