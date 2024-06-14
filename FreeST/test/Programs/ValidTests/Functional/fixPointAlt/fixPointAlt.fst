
{- |
Module      :  fixPointAlt
Description :  An alternative encoding of the fixed-point Z combinator. Used to calculate a factorial
Copyright   :  (c) Bernardo Almeida

The fixed-point Z combinator: Z=\f.(\x.f(\z.xxz))(\y.f(\z.yyz)) is
used to calculate the factorial of 8

-}

-- This is the Y-combinator. It never halts in a strict language
-- fix' : forall a . ((a -> a) -> (a -> a)) -> (a -> a)
-- fix' f = f (fix' @a f) 

fix' : ((a -> a) -> (a -> a)) -> a -> a
fix' f x = f (fix' @a f) x

fact : Int -> Int
fact = fix'  @Int (\f:(Int -> Int) -> (\n:Int ->
  if n == 0 then 1 else n * f (n - 1)))

main : Int
main = fact 5
