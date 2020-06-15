--type Fix = forall t:TU => (t -> t) -> t

-- fix : Fix

-- fix : ((Int -> Int) -> (Int -> Int)) -> (Int -> Int)
-- fix f = (\x:(rec a.a -> (Int -> Int)) -> f(x x))
--         (\x:(rec a.a -> (Int -> Int)) -> f (x x))

-- Z combinator, the Y combinator (above) loops in call by name
-- Z=λf.(λx.f(λz.xxz))(λy.f(λz.yyz))
fixZcomb : ((Int -> Int) -> (Int -> Int)) -> (Int -> Int)
fixZcomb f =
  (\x:(rec a.a -> (Int -> Int)) -> f(\z:Int -> x x z))
  (\x:(rec a.a -> (Int -> Int)) -> f(\z:Int -> x x z))


fact : Int -> Int
fact = fixZcomb (\f:(Int -> Int) -> (\n:Int ->
  if n == 0 then 1 else n * f (n -1)))

main : Int
main = fact 8
