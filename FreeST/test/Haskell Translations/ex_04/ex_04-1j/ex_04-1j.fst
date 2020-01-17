-- IV exercise 1j

data IntList = End | List Int IntList

posicoes : IntList -> Int -> IntList
posicoes list m = posicoes' list m 0

posicoes' : IntList -> Int -> Int -> IntList
posicoes' list m i =
    case list of {
        End -> End,
        List x rest -> if mod x m == 0 
                            then List i (posicoes' rest m (i+1))
                            else posicoes' rest m (i+1)
    }

main : IntList
main = posicoes (List 1 (List 3 (List 6 (List 2 (List 5 (List 15 (List 3 (List 5 (List 7 (List 18 End)))))))))) 3
-- result = List 1 (List 2 (List 5 (List 6 (List 9 End))))