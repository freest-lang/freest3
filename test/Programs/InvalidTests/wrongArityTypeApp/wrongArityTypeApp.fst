fst' : forall a: TU, b: TU => (a, b) -> a
fst' p = let x, _ = p in x

main: Int
main = fst' [Bool] (True, 7)
