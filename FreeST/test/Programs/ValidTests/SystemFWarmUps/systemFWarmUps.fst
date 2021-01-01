{- |
Module      : SystemFWarmUps
Description : Examples from TAPL, Chapter 23, Universal Types
Copyright   : (c) Vasco T. Vasconcelos, 31 dec 2020
-}

id : ∀ a => a -> a
id = (Λ a => (λ x:a -> x))

five : Int
five = id [Int] 5

double : ∀ a => (a -> a) -> a -> a
double = (Λ a => (λ f:(a->a) -> (λ x:a -> f (f x))))

doubleInt : (Int -> Int) -> Int -> Int
doubleInt = double [Int]

seven : Int
seven = doubleInt (λ x:Int -> x + 2) 3

doubleIntArrowInt :
  ((Int -> Int) -> (Int -> Int)) -> (Int -> Int) -> (Int -> Int)
doubleIntArrowInt = double [Int -> Int]

thirteen : Int
thirteen = doubleIntArrowInt doubleInt (λ x:Int -> x + 2) 5

quadruple : ∀ a => (a -> a) -> a -> a
quadruple = (Λ a => double [a -> a] (double [a]))

main : Int
main = quadruple [Int] (λ x:Int -> x + 2) 3
