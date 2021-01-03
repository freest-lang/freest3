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

type Bin = ∀a => a -> (a -> a) -> (a -> a) -> a

zero : Bin
zero z s0 s1 = z

zero' : Bin
zero' z s0 s1 = s0 z

one : Bin
one z s0 s1 = s1 z

two : Bin
two z s0 s1 = s0 $ s1 z -- 10

three : Bin
three z s0 s1 = s1 $ s1 z -- 11

four : Bin
four z s0 s1 = s0 $ s0 $ s1 $ z -- 100

fifteen : Bin
fifteen z s0 s1 = s1 $ s1 $ s1 $ s1 $ z -- 11111

isZero : Bin -> Bool
isZero n = n [Bool] True (λ_:Bool -> False) (λ_:Bool -> False)

toInt : Bin -> Int
toInt n = n [Int] 0 (λx:Int -> 2 * x) (λx:Int -> 2 * x + 1)

main : Bool
main = isZero fifteen
-- main = toInt fifteen

-- TO BE CONTINUED
