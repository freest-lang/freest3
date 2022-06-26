
data List = Nil | Cons Int List

f : List -> Int
f Nil = -1
f (Cons x Nil) = x
f (Cons x xs)  = x*10

main : Int
main = f $ Cons 1 $ Cons 2 $ Cons 3 Nil

-- f' : List -> Int
-- f' a = 
--   case a of {
--     Nil -> -1,
--     Cons b c ->
--       case c of {
--         Nil -> b,
--         Cons _ _ -> b*10
--       }
--   }