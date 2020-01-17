-- VIII exercise 3c

data Str = End | Str Char Str
data IntList = IE | IL Int IntList
data StrList = StrE | StrL Str StrList

mapM_' : (Int -> Str) -> IntList -> StrList
mapM_' f list =
    case list of {
        IE -> StrE,
        IL x rest -> StrL (f x) (mapM_' f rest)
    }

printEven : Int -> Str
printEven x = 
    if mod x 2 == 0
        then par
        else impar

par : Str
par = Str 'P' (Str 'a' (Str 'r' End))

impar : Str
impar = Str 'I' (Str 'm' (Str 'p' (Str 'a' (Str 'r' End))))

list : IntList
list = IL 1 (IL 2 (IL 3 (IL 4 (IL 5 IE))))

main : StrList
main = mapM_' printEven list
--result = StrL (Str 'I' (Str 'm' (Str 'p' (Str 'a' (Str 'r' End))))) (StrL (Str 'P' (Str 'a' (Str 'r' End))) (StrL (Str 'I' (Str 'm' (Str 'p' (Str 'a' (Str 'r' End))))) (StrL (Str 'P' (Str 'a' (Str 'r' End))) (StrL (Str 'I' (Str 'm' (Str 'p' (Str 'a' (Str 'r' End))))) StrE))))