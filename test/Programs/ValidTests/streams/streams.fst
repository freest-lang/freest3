{-
"A more useful variant of the Hungry type above is the type Stream of func- tions that can consume an arbitrary number of unit values, each time returning a pair of a number and a new stream."
-}

-- This should be in the prelude

fst : forall a:TU, b:TU => (a, b) -> a
fst p = let (x, _) = p in x

snd : forall a:TU, b:TU => (a, b) -> b
snd p = let (_, y) = p in y

-- A more useful variant of the Hungry type above is the type Stream
-- of func- tions that can consume an arbitrary number of unit values,
-- each time returning a pair of a number and a new stream.
type Stream = rec x:TU. () -> (Int, x)

-- We can define two “destructors” for streams; if s is a stream, then
-- hd s is the first number it returns when we pass it unit.

hd : (rec x:TU. () -> (Int, x)) -> Int
hd s = fst [Int, (rec x:TU. () -> (Int, x))] (s ())

-- Similarly, tl s is the new stream that we obtain when we pass unit to s.

tl : (rec x:TU. () -> (Int, x)) -> (rec x:TU. () -> (Int, x))
tl s = snd [Int, (rec x:TU. () -> (Int, x))] (s ())

-- Construct a stream

upFrom : Int -> rec x:TU. () -> (Int, x)
upFrom n  = (\ _:() -> (n, upFrom (n + 1)))

-- The third element in the upFrom 0 stream

main : Int
main = hd (tl (tl (tl (upFrom 0))))
