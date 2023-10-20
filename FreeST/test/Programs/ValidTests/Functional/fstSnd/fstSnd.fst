fst' : forall a . forall b . (a, b) -> a
snd' : forall a . forall b . (a, b) -> b

fst' p = let (x, _) = p in x
snd' p = let (_, y) = p in y

main : Int
main = fst' @Int @Char (5, 'h') + snd' @Bool @Int (True, 7)
