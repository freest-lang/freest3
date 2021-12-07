{- |
Module      : SystemFPairs
Description : Pairs in System F
Copyright   : (c) Vasco T. Vasconcelos, 12 nov 2021

Church Encoding _ Sums
as per Practical Foundations for Programming Languages, Robert Harper, 2nd edition, page 141
-}

type Sum a b = ∀c . (a -> c) -> (b -> c) -> c

inl : ∀a b . a -> (∀c. (a -> c) -> (b -> c) -> c)
inl e = Λc => λx:(a->c) -> λ_:(b->c) -> x e

inr : ∀a b . b -> (∀c. (a -> c) -> (b -> c) -> c)
inr e = Λc => λ_:(a->c) -> λy:(b->c) -> y e

cases : ∀a b c . (∀c . (a -> c) -> (b -> c) -> c) -> (a -> c) -> (b -> c) -> c
cases e cl cr = e [c] cl cr

fromL : ∀a b . (∀c . (a -> c) -> (b -> c) -> c) -> a -> a
fromL l v = cases [a, b, a] l (id [a]) (λ_:b -> v)

fromR : ∀a b . (∀c . (a -> c) -> (b -> c) -> c) -> b -> b
fromR r v = cases [a, b, b] r (λ_:a -> v) (id [b])

-- Examples

type IntPlusBool = ∀c . (Int -> c) -> (Bool -> c) -> c

-- inject an Int in a Int+Bool sum
inInt : Int -> IntPlusBool
inInt n = inl [Int] [Bool] n

-- inject a Bool in a Int+Bool sum
inBool : Bool -> IntPlusBool
inBool b = inr [Int] [Bool] b

-- convert a Int+Bool sum into an Int
main : Int
main = fromL [Int, Bool] (inInt 324) 0

-- same w/o using fromL
toInt : Int
toInt = cases [Int, Bool, Int] (inInt 324) (id [Int]) (λ_:Bool -> 0)

-- convert a Int+Bool sum into a Bool
main' : Bool
main' = fromR [Int, Bool] (inBool True) False

toBool : Bool
toBool = cases [Int, Bool, Bool] (inBool True) (λ_:Int -> False) (id [Bool])


