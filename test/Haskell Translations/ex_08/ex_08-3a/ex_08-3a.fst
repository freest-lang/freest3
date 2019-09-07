-- VIII exercise 3a

data Str = End | Str Char Str

printEven : Int -> Str
printEven x = 
    if mod x 2 == 0
        then par
        else impar

par : Str
par = Str 'P' (Str 'a' (Str 'r' End))

impar : Str
impar = Str 'I' (Str 'm' (Str 'p' (Str 'a' (Str 'r' End))))

data DoubleStr = DS Str Str

main : DoubleStr
main = DS (printEven 1) (printEven 2)
--result = DS (Str 'I' (Str 'm' (Str 'p' (Str 'a' (Str 'r' End))))) (Str 'P' (Str 'a' (Str 'r' End)))