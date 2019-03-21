dot : forall a : TU, b : TU, c : TU => (b -> c) -> (a -> b) -> a -> c
dot f g x = f[a,b,c] (g[a,b,c] x)

double : Int -> Int
double x = 2 * x

isZero : Int -> Bool
isZero x = x == 0

main : Bool
main = dot [Int, Int, Bool] isZero double 7
-- dot [Int->Bool] [Int->Int] [Int] isZero double 7


-- a : Int
-- a = 2 mod 3

