
dot :: forall a :: TU, b :: SU, c :: TU => (b -> c) -> (a -> b) -> a -> c
dot f g x = f (g x)

double :: Int -> Int
double x = 2 * x

isZero :: Int -> Bool
isZero x = x == 0

start :: Bool
start = dot [Int] [Int] [Bool] isZero double 7
-- dot [Int->Bool] [Int->Int] [Int] isZero double 7

