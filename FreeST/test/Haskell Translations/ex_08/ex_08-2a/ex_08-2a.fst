-- VIII exercise 2a

data Str = End | Str Char Str

palindrome : Str -> Str
palindrome str = 
    if equals str (reverseStr str)
        then Str 'S' (Str 'i' (Str 'm' End))
        else Str 'N' (Str 'a' (Str 'o' End))

reverseStr : Str -> Str
reverseStr str = reverseStr' str End

reverseStr' : Str -> Str -> Str
reverseStr' s1 s2 = 
    case s1 of {
        End -> s2,
        Str c rest -> reverseStr' rest (List c s2)
    }

equals : Str -> Str -> Bool
equals s1 s2 =
    case s1 of {
        End -> 
            case s2 of {
                End -> True,
                Str _ _ -> False
            },
        Str c1 r1 -> 
            case s2 of {
                End -> False,
                Str c2 r2 -> 
                    if c1 /= c2         -- Char's cannot be tested equal
                        then False
                        else equals r1 r2
            }
    }

word1 : Str
word1 = Str 'p' (Str 'a' (Str 'l' (Str 'i' (Str 'n' (Str 'd' (Str 'r' (Str 'o' (Str 'm' (Str 'o' End)))))))))

word2 : Str
word2 = Str 'r' (Str 'e' (Str 'v' (Str 'i' (Str 'v' (Str 'e' (Str 'r' End))))))

main : Str
main = not (palindrome word1) && palindrome word2
-- expected result = True
