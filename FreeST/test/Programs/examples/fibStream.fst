{-

TAPL, page 271

20.1.2 Exercise [Recommended, **]: Define a stream that yields
successive elements of the Fibonacci sequence (1, 1, 2, 3, 5, 8, 13,
...).

-}

{- Haskell

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fib n = fibs !! n
-}

-- These should be in the prelude

fst : forall a:TL, b:TU => (a, b) -> a
fst p = let (x, _) = p in x

snd : forall a:TU, b:TL => (a, b) -> b
snd p = let (_, y) = p in y

-- Stream

type Stream = () -> (Int, Stream)

-- Deconstructors

hd : Stream -> Int
hd s = fst  @Int @Stream (s ())

tl : Stream -> Stream
tl s = snd  @Int @Stream (s ())

nth : Int -> Stream -> Int
nth n s = if n == 0 then hd s else nth (n - 1) (tl s)

-- Constructor

mk : (Int, Stream) -> (() -> (Int, Stream))
mk x = (\_:() -> x)

-- Fibs  

fibs : Stream
fibs = mk (1, (mk (1, (zipWith (+) fibs (tl fibs)))))

zipWith : (Int -> Int -> Int) -> Stream -> Stream -> Stream
zipWith f s1 s2 = mk (f (hd s1) (hd s2), zipWith f (tl s1) (tl s2))

-- Main

main : Int
main = nth 8 fibs
