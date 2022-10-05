data List = Nil | Cons Int List
data T = A | B | C

f : T -> List -> Int
f A Nil = 0
f B (Cons _ Nil) = 1
f C (Cons _ (Cons x Nil)) = 2
f A xs = 3
f x (Cons _ Nil) = 3

main : Int
main = f A Nil                              -- 0
     + f B (Cons 0 Nil)                     -- 1
     + f A (Cons 0 $ Cons 1 $ Cons 2 Nil)   -- 3
     + f A (Cons 0 Nil)                     -- 3