id : forall a => a -> a
id x = x

f : (Char -> Char) -> Char
f g = g 'a'

main : Char
main = f id[Char]