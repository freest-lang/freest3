-- VIII exercise 2b

data Str = End | Str Char Str
data StrList = E | S Str StrList

sim : Str
sim = Str 'S' (Str 'i' (Str 'm' End))

nao : Str
nao = Str 'N' (Str 'a' (Str 'o' End))

palindrome : Str -> Str
palindrome str = 
    if equals str (reverseStr str)
        then sim
        else nao

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

map' : (Str -> Str) -> StrList -> StrList
map' f strList =
    case strList of {
        E -> E,
        S s rest -> S (f s) (map' f rest) 
    }

main : StrList
main = map' palindrome (L sim (L nao (L word1 (L word2 End))))
-- expected result = S (Str 'N' (Str 'a' (Str 'o' End))) (S (Str 'N' (Str 'a' (Str 'o' End))) (S (Str 'N' (Str 'a' (Str 'o' End))) (S (Str 'S' (Str 'i' (Str 'm' End))) E )))
