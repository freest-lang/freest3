{-
Examples from TAPL, Chapter 23, Universal Types
-}

-- Warm-ups

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

eleven : Int
eleven = quadruple [Int] (λ x:Int -> x + 2) 3

-- main : Int
-- main = eleven

-- Polymorphic Lists. Requires type constructor List

-- Church Encodings

type BoolC = ∀ b => b -> b -> b

trueC : BoolC
trueC = (Λ a => (λ t:a -> (λ f:a -> t)))

falseC : BoolC
falseC = (Λ a => (λ t:a -> (λ f:a -> f)))

notC : BoolC -> BoolC
notC = (λ b: BoolC -> (Λ a => (λ t:a -> (λ f:a -> b [a] f t))))

-- Abbreviated versions

trueS : BoolC
trueS t _ = t

falseS : BoolC
falseS _ f = f

ifC : ∀ a => BoolC -> a -> a -> a
ifC b e1 e2 = b [a] e1 e2

notS : BoolC -> BoolC
notS b = (Λ a => (λ t:a -> (λ f:a -> b [a] f t)))

notS' : BoolC -> BoolC
notS' b = ifC [BoolC] b falseC trueC

orC : BoolC -> BoolC -> BoolC
orC b1 b2 = ifC [BoolC] b1 trueC b2

andC : BoolC -> BoolC -> BoolC
andC b1 b2 = ifC [BoolC] b1 b2 falseC

-- Testing

toBool : BoolC -> Bool
toBool b = b [Bool] True False

toBit : BoolC -> Int
toBit b = b [Int] 1 0

ifInt : BoolC -> Int -> Int -> Int
ifInt = ifC [Int]

main : Int
main = ifInt (notC trueC) 1 2

-- main : Bool
-- main = toBool $ andC (orC falseC trueC) (notS' falseC)
