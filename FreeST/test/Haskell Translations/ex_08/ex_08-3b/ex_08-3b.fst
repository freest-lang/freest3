-- VIII exercise 3b

data Str = End | Str Char Str
data List = E | L Int List
data StrList = StrE | StrL Str StrList

printEven : Int -> Str
printEven x = 
    if mod x 2 == 0
        then par
        else impar

par : Str
par = Str 'P' (Str 'a' (Str 'r' End))

impar : Str
impar = Str 'I' (Str 'm' (Str 'p' (Str 'a' (Str 'r' End))))

showParity : List -> StrList
showParity list =
    case list of {
        E -> StrE,
        L x rest -> StrL (printEven x) (showParity rest)
    }

list : List
list = L 1 (L 2 (L 3 (L 4 (L 5 E))))

main : StrList
main = showParity list
--result = StrL (Str 'I' (Str 'm' (Str 'p' (Str 'a' (Str 'r' End))))) (StrL (Str 'P' (Str 'a' (Str 'r' End))) (StrL (Str 'I' (Str 'm' (Str 'p' (Str 'a' (Str 'r' End))))) (StrL (Str 'P' (Str 'a' (Str 'r' End))) (StrL (Str 'I' (Str 'm' (Str 'p' (Str 'a' (Str 'r' End))))) StrE))))