--type Fix = forall t:TU => (t -> t) -> t

-- fix : Fix
fix : ((Int -> Int) -> (Int -> Int)) -> (Int -> Int)
fix f = (\x:(rec a.a -> (Int -> Int)) -> f (x x))
        (\x:(rec a.a -> (Int -> Int)) -> f (x x))

fact : Int -> Int
fact = fix (\f:(Int -> Int) -> (\n:Int ->
  if n == 0 then 1 else n * f (n -1)))

main : Int
main = fact 8
