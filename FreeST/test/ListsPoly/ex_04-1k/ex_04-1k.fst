-- IV exercise 1k

concat' : [Char] -> [Char] -> [Char]
concat' s1 s2 = 
    case s1 of {
        [] -> s2,
        c :: rest -> c :: (concat' rest s2)
    }

frase : Int -> [(Int,[Char])] -> [Char]
frase x list =
    case list of {
        [] -> [],
        y :: string rest -> 
            if x == y
                then concat' string (frase x rest)
                else frase x rest
    }

main : [Char]
main = frase 3 [ (3,['a','b']), (1,['c','d']), (3,['e','f']) ]
-- result = ['a','b','e','f']