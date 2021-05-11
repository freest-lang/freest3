{- |
Module      : SystemFWarmUps
Description : Examples from TAPL, Chapter 23, Universal Types
Copyright   : (c) Vasco T. Vasconcelos, 31 dec 2020
-}

five : Int
five = id [Int] 5

double : ∀ a . (a -> a) -> a -> a
double = Λ a => λ f:(a->a) x:a -> f (f x)

doubleInt : (Int -> Int) -> Int -> Int
doubleInt = double [Int]

seven : Int
seven = doubleInt (λ x:Int -> x + 2) 3

doubleIntArrowInt :
  ((Int -> Int) -> (Int -> Int)) -> (Int -> Int) -> (Int -> Int)
doubleIntArrowInt = double [Int -> Int]

thirteen : Int
thirteen = doubleIntArrowInt doubleInt (λ x:Int -> x + 2) 5

quadruple : ∀ a . (a -> a) -> a -> a
quadruple = Λ a => λ f:(a->a) -> double [a -> a] (double [a]) f

{-
Note: One cannot apply eta-reduction in the code above to get rid
of f and obtain:

  quadruple = Λ a => double [a -> a] (double [a])[a])

for the body of the type application (that is, n [a->a] (m [a]))
must be a value. Once we apply eta-expansion we obtain Λ a => λ f...
which is OK for λ f... is a value.
-}

main : Int
main = quadruple [Int] (λ x:Int -> x + 2) 3
