data List = Nil | Cons Int List

sum : List -> Int
sum xs = case xs of {
    Nil -> 0,
    Cons x xs' -> x + sum xs'
  }

filter : (Int -> Bool) -> List -> List
filter p xs = case xs of {
    Nil -> Nil,
    Cons x xs' -> if p x then Cons x (filter p xs') else filter p xs'
  }

map : (Int -> Int) -> List -> List
map f xs = case xs of {
    Nil -> Nil,
    Cons x xs' -> Cons (f x) (map f xs')
  }

xs : List
xs = Cons 7 $ Cons 8 $ Cons (-1) $ Cons 1 $ Cons 6 $ Cons 5 Nil

main : Int
main = sum $ filter (λx:Int -> x > 10) $ map (λy:Int -> y * 2) xs
