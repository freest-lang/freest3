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

selfApp : ∀ a => (a -> a) -> a -> a
selfApp = (Λ a => double [a -> a] (double [a]))

quadruple : ∀ a => (a -> a) -> a -> a
quadruple = (Λ a => double [a -> a] (double [a]))

eleven : Int
eleven = quadruple [Int] (λ x:Int -> x + 2) 3

-- Polymorphic Lists. Requires type constructor List

-- Church Encodings

type CBool = ∀ a => a -> a -> a

tru : ∀ a => a -> a -> a
tru = (Λ a => (λ t:a -> (λ f: a -> t)))

fls : ∀ a => a -> a -> a
fls = (Λ a => (λ t:a -> (λ f: a -> f)))

nott : (∀ a => a -> a -> a) -> (∀ a => a -> a -> a)
nott = (λ b: (∀ a => a -> a -> a) -> (Λ c => (λ t:c -> (λ f:c -> b [c] f t))))

-- short version

truS : ∀ a => a -> a -> a
truS t _ = t

flsS : ∀ a => a -> a -> a
flsS _ f = f

notS : (∀ a => a -> a -> a) -> (∀ a => a -> a -> a)
notS b = (Λ c => (λ t:c -> (λ f:c -> b [c] f t)))

main : Int
main = eleven
