{- |
Module      :  FixZcombinator
Description :  Using fixed-point Z combinator to calculate a factorial
Copyright   :  (c) Bernardo Almeida, Vasco T. Vasconcelos, Andreia Mordido

The fixed-point Z combinator: Z=\f.(\x.f(\z.xxz))(\y.f(\z.yyz)) is
used to calculate the factorial of 8

-}


fixZcomb : ((Int -> Int) -> (Int -> Int)) -> (Int -> Int)
fixZcomb f =
  (\x:(rec a.a -> (Int -> Int)) -> f(\z:Int -> x x z))
  (\x:(rec a.a -> (Int -> Int)) -> f(\z:Int -> x x z))


fact : Int -> Int
fact = fixZcomb (\f:(Int -> Int) -> (\n:Int ->
  if n == 0 then 1 else n * f (n -1)))

main : Int
main = fact 8
