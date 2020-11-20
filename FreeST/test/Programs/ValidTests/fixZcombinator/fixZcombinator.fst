{- |
Module      :  FixZcombinator
Description :  Using fixed-point Z combinator to calculate a factorial
Copyright   :  (c) Bernardo Almeida, Andreia Mordido, Vasco T. Vasconcelos

The fixed-point Z combinator: Z=\f.(\x.f(\z.xxz))(\y.f(\z.yyz)) is
used to calculate the factorial of 8

-}

fixZcomb : forall a => ((a -> a) -> (a -> a)) -> (a -> a)
fixZcomb f =
  (\x:(rec b.b -> (a -> a)) -> f(\z:a -> x x z))
  (\x:(rec b.b -> (a -> a)) -> f(\z:a -> x x z))

fact : Int -> Int
fact = fixZcomb [Int] (\f:(Int -> Int) -> (\n:Int ->
  if n == 0 then 1 else n * f (n - 1)))

main : Int
main = fact 8
