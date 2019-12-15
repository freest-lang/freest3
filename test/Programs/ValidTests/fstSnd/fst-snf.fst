fst' : forall a: TU, b: TU => L(a, b) -> a
snd' : forall a: TU, b: TU => L(a, b) -> b

fst' p = let (x, _) = p in x
snd' p = let (_, y) = p in y

main : Int
main = fst'[Int, Char] L(5, 'h') + snd'[Bool, Int] L(True, 7)
