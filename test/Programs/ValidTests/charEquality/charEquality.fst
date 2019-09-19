

main : Bool
main = equalChars 'a' 'a'


equalChars : Char -> Char -> Bool
equalChars c1 c2 = ord c1 == ord c2
