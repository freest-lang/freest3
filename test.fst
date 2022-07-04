

data Pair = Pair Int Int
data Pairs = N | P Pair Pairs

f : Pairs -> Int
f N             = 0
f (P pair N)    = 1
f (P pair rest) = f rest

main : Int
main = f $ P (Pair 1 1) $ P (Pair 2 2) N