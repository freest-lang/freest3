{- |
Module      : SystemFPairs
Description : Pairs in System F
Copyright   : (c) Vasco T. Vasconcelos, 2 jan 2021

Church Encoding _ Pairs
-}

type Pair a b = ∀c . (a -> b -> c) -> c

fst' : ∀a b . (∀c . (a -> b -> c) -> c) -> a
fst' p = p [a] (λx:a _:b -> x)

snd' : ∀a b . (∀c . (a -> b -> c) -> c) -> b
snd' p = p [b] (λ_:a y:b -> y)

pair : ∀a b . a -> b -> ∀c . (a -> b -> c) -> c
pair x y = Λc => λz:(a -> b -> c) -> z x y

intBoolPair : Int -> Bool -> ∀c . (Int -> Bool -> c) -> c 
intBoolPair = pair [Int Bool]

main : Char
main = snd' [Int Char] $
       fst' [∀c . (Int -> Char -> c) -> c] [Bool] $
       pair [∀c . (Int -> Char -> c) -> c] [Bool] (pair [Int Char] 5 'c') False

-- main = snd' [Int] [Bool] $ pair [Int] [Bool] 5 False
