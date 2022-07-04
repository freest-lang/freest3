-- VII exercise 14

data Str = End | Str Char Str

toStringChar : Char -> Str
toStringChar c = Str c End

dimensionChar : Char -> Int
dimensionChar _ = 1

toStringBool : Bool -> Str
toStringBool b 
    | b         = Str 'T' (Str 'r' (Str 'u' (Str 'e' End)))
    | otherwise = Str 'F' (Str 'a' (Str 'l' (Str 's' (Str 'e' End))))

dimensionBool : Bool -> Int
dimensionBool b | b = 4 | otherwise = 5 -- dimension of "True" and "False"

data List = E | List Int List

toStringList : List -> Str
toStringList E             = End
toStringList (List x rest) = concatStr (translateN x) (toStringList rest)

dimensionList : List -> Int
dimensionList E             = 0
dimensionList (List _ rest) = 1 + (dimensionList rest)

concatStr : Str -> Str -> Str
concatStr End          s2 = s2
concatStr (Str x rest) s2 = Str x (concatStr rest s2)

translateN : Int -> Str
translateN x 
    | x == 0 = Str '0' End
    | x <  0 = Str '-' (reverseStr (translateN' (-x))) 
    | otherwise = reverseStr (translateN' x)

translateN' : Int -> Str
translateN' x
    | x == 0    = End
    | otherwise = Str (getNum (mod x 10)) (translateN' (div x 10))

reverseStr : Str -> Str
reverseStr str = reverseStr' str End

reverseStr' : Str -> Str -> Str
reverseStr' End          acc = acc
reverseStr' (Str x rest) acc = reverseStr' rest (Str x acc)

getNum : Int -> Char
getNum x =
    if x == 0 then '0' else
    if x == 1 then '1' else
    if x == 2 then '2' else
    if x == 3 then '3' else
    if x == 4 then '4' else
    if x == 5 then '5' else
    if x == 6 then '6' else
    if x == 7 then '7' else
    if x == 8 then '8' else
    if x == 9 then '9' else
    'N'

data Pair = Pair Int Int
data Pairs = N | P Pair Pairs

toStringPairs : Pairs -> Str
toStringPairs ps = Str '{' (toStringPairs' ps)

dimensionPairs : Pairs -> Int
dimensionPairs N          = 0
dimensionPairs (P _ rest) = 1 + (dimensionPairs rest)

toStringPairs' : Pairs -> Str
toStringPairs' N             = End
toStringPairs' (P pair N)    = concatStr (toStringPair pair) (Str '}' End)
toStringPairs' (P pair rest) = concatStr (toStringPair pair) (Str ',' (toStringPairs' rest))

toStringPair : Pair -> Str
toStringPair (Pair a b) = concatStr (Str '(' (translateN a)) (concatStr (Str ',' (translateN b)) (Str ')' End))

main : Str
main = 
    let a = toStringBool True in
    let b = toStringChar 'a'  in
    let c = toStringList (List 1 (List 100 (List 50 (List 20 E)))) in
    let d = toStringPair (Pair 10 15) in 
    let e = toStringPairs (P (Pair 1 1) (P (Pair 2 2) (P (Pair 3 3) N))) in
    concatStr (toStringBool False) (concatStr b (concatStr c (concatStr d e)))
--result = Str 'F' (Str 'a' (Str 'l' (Str 's' (Str 'e' (Str 'a' 
--         (Str '1' (Str '1' (Str '0' (Str '0' (Str '5' (Str '0' 
--         (Str '2' (Str '0' (Str '(' (Str '1' (Str '0' (Str ',' 
--         (Str '1' (Str '5' (Str ')' (Str '{' (Str '(' (Str '1' 
--         (Str ',' (Str '1' (Str ')' (Str ',' (Str '(' (Str '2' 
--         (Str ',' (Str '2' (Str ')' (Str ',' (Str '(' (Str '3' 
--         (Str ',' (Str '3' (Str ')' (Str '}' End))))))))))))))
--         )))))))))))))))))))))))))