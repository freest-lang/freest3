
data List = Nil | List Int List

data T = A T | B

f : T -> Int
f t = 
  case t of {
    A x -> 0,
    (A B) -> 1,
    _ | 1 == 2 -> 2
      | _ -> 2
  }

main : Int
main = f $ B
