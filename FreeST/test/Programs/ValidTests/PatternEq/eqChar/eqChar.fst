

f : Char -> Char -> Bool
f 'a' 'b' = False
f 'a' 'a' = True

main : Bool
main = (f 'a' 'b') || (f 'a' 'a')