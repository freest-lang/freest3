{- |
Module      : SystemFBooleans
Description : Examples from TAPL, Chapter 23, Universal Types
Copyright   : (c) Vasco T. Vasconcelos, 31 dec 2020

Church Encoding _ Boolean Values
-}

type BoolC = ∀ b . b -> b -> b

trueC, falseC : BoolC

trueC = Λ a => λ t:a -> λ f:a -> t

falseC = Λ a => λ t:a -> λ f:a -> f

notC : BoolC -> BoolC
notC = λ b: BoolC -> Λ a => λ t:a -> λ f:a -> b  @a f t

-- Abbreviated versions of the above

type BoolC' b = b -> b -> b

trueC', falseC': BoolC

trueC' t _ = t

falseC' _ f = f

notC' : BoolC -> BoolC
notC' b = Λ a => λ t:a f:a -> b  @a f t

-- Destructor

cond : ∀ a . BoolC -> a -> a -> a
cond b e1 e2 = b  @a e1 e2

-- Boolean ops based on the conditional

notC'' : BoolC -> BoolC
notC'' b = cond  @BoolC b falseC trueC

orC, andC : BoolC -> BoolC -> BoolC
orC b1 b2 = cond  @BoolC b1 trueC b2

andC b1 b2 = cond  @BoolC b1 b2 falseC

-- Testing

toBool : BoolC -> Bool
toBool b = b  @Bool True False

toBit : BoolC -> Int
toBit b = b  @Int 1 0

ifInt : BoolC -> Int -> Int -> Int
ifInt = cond  @Int

-- main : Int
-- main = ifInt (notC trueC) 1 2

main : Bool
main = toBool $ andC (orC falseC trueC) (notC falseC)
