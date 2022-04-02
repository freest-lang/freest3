fst' : forall a: TL . forall b: TU . (a, b) -> a
snd' : forall a: TU . forall b: TL . (a, b) -> b

fst' p = let (x, _) = p in x
snd' p = let (_, y) = p in y

main : Int
main = fst' @Int @Char (5, 'h') + snd' @Bool @Int (True, 7)
