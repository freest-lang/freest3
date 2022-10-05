data List = Nil | Cons Int List

f : List -> Int
f xs =
  case xs of {
    Nil                   -> 0,
    Cons _ Nil            -> 1,
    (Cons _ (Cons x Nil)) -> 2,
    xs                    -> 3,
    Cons _ Nil            -> 3
  }

g : List -> Int
g xs =
  case xs of {
    Nil -> 0,
    Cons _ xs -> 
    case xs of {
      Nil -> 1,
      Cons _ Nil -> 2,
      Cons _ _ -> 3
    }
  }

main : Int
main = f Nil                              -- 0
     + f (Cons 0 Nil)                     -- 1
     + f (Cons 0 $ Cons 1 $ Cons 2 Nil)   -- 3
     + f (Cons 0 Nil)                     -- 1
     + g Nil                              -- 0
     + g (Cons 0 Nil)                     -- 1
     + g (Cons 0 $ Cons 1 $ Cons 2 Nil)   -- 3
     + g (Cons 0 Nil)                     -- 1