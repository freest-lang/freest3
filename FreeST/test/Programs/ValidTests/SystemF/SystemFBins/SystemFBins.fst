{- |
Module      : SystemFBins
Description : Natural numbers as sequences of zeros and ones.
Copyright   : (c) Vasco T. Vasconcelos

Church Encoding _ Binary natural numbers.

Jean-Yves Girard
The Blind Spot
European Mathematical Society, 2011

"The previous integers are <<Cro-Magnon integer>>, anterior to the Babylonian numeration. A more modern version of integers requires finite sequences of zeros and ones." (page 119)
-}

type Bin = ∀a . a -> (a -> a) -> (a -> a) -> a

zero, zero', one, two, three, four, fifteen : Bin

zero z s0 s1 = z

zero' z s0 s1 = s0 z

one z s0 s1 = s1 z

two z s0 s1 = s0 $ s1 z -- 10

three z s0 s1 = s1 $ s1 z -- 11

four z s0 s1 = s0 $ s0 $ s1 $ z -- 100

fifteen z s0 s1 = s1 $ s1 $ s1 $ s1 $ z -- 11111

isZero : Bin -> Bool
isZero n = n @Bool True (λ_:Bool -> False) (λ_:Bool -> False)

toInt : Bin -> Int
toInt n = n @Int 0 (λx:Int -> 2 * x) (λx:Int -> 2 * x + 1)

-- succ' : Bin -> Bin
-- succ' n = Λa => (one @a) (λs0:(a->a) -> s0 n@a) (λs1:(a->a) -> s1 n@a)

-- succ' : Bin -> Bin
-- succ' n = n @Bin
--           one
--           (λz:Bin -> λs0:(Bin->Bin) -> λs1:(Bin->Bin) -> s0 n)
--           (λz:Bin -> λs0:(Bin->Bin) -> λs1:(Bin->Bin) -> s1 n)

main : Bool
main = isZero fifteen
-- main = toInt fifteen

-- TO BE CONTINUED
