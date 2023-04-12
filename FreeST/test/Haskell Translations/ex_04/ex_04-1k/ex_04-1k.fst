-- IV exercise 1k

data CharList = CEnd | CList Char CharList
data PairList = End | List Int CharList PairList

concat' : CharList -> CharList -> CharList
concat' s1 s2 = 
    case s1 of {
        CEnd -> s2,
        CList c rest -> CList c (concat' rest s2)
    }

frase : Int -> PairList -> CharList
frase x list =
    case list of {
        End -> CEnd,
        List y string rest -> if x == y
                                then concat' string (frase x rest)
                                else frase x rest
    }

main : CharList
main = frase 3 (List 3 (CList 'a' (CList 'b' CEnd)) (List 1 (CList 'c' (CList 'd' CEnd)) (List 3 (CList 'e' (CList 'f' CEnd)) End)))
-- result = CList 'a' (CList 'b' (CList 'e' (CList 'f' CEnd)))
