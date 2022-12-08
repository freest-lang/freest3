data List = Nil | Cons Int List

f : List -> Int
f Nil                   = 0
f (Cons _ Nil)          = 1
f (Cons _ (Cons x Nil)) = 2
f xs                    = 3
f (Cons _ Nil)          = 3

g : List -> Int
g Nil         = 0
g (Cons _ xs) =
  case xs of {
    Nil -> 1,
    Cons _ Nil -> 2,
    Cons _ _ -> 3
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
