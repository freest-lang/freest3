fst' : forall a: TU, b: TU => (a, b) -> a
snd' : forall a: TU, b: TU => (a, b) -> b

fst' p = let x, y = p in x
snd' p = let x, y = p in y

main : Int
main = fst' (5, 'h') + snd' (True, 7)
