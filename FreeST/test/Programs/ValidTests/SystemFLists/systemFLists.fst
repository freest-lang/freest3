{- |
Module      : SystemFLists
Description : Pairs in System F
Copyright   : (c) Vasco T. Vasconcelos, 2 jan 2021

Church Encoding _ Lists
-}

type List a = ∀r . (a -> r -> r) -> r -> r

nil : ∀a r . (a -> r -> r) -> r -> r
nil = Λa r => λc:(a -> r -> r) n:r -> n

cons : ∀a . a -> (∀r . (a -> r -> r) -> r -> r) -> (∀r . (a -> r -> r) -> r -> r)
cons = Λa => λhd:a tl:(∀r . (a -> r -> r) -> r -> r) ->
          Λr =>  λc:(a -> r -> r) n:r -> c hd (tl [r] c n)

null : ∀a . (∀r . (a -> r -> r) -> r -> r) -> Bool
null = Λa => λl:(∀r . (a -> r -> r) -> r -> r) -> l [Bool] (λhd:a tl:Bool -> False) True

main : Bool
main = null [Char] (nil [Char])
