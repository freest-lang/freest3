fst' : forall a: 1T . forall b: *T . (a, b) -> a
snd' : forall a: *T . forall b: 1T . (a, b) -> b

fst' p = let (x, _) = p in x
snd' p = let (_, y) = p in y

main : Int
main = fst'[Int][Char] (5, 'h') + snd'[Bool][Int] (True, 7)
