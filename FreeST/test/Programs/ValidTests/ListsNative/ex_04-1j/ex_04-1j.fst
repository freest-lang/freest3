-- IV exercise 1j

posicoes : [Int] -> Int -> [Int]
posicoes list m = posicoes' list m 0

posicoes' : [Int] -> Int -> Int -> [Int]
posicoes' list m i =
    case list of {
        [] -> [],
        x :: rest -> 
            if mod x m == 0 
                then i :: (posicoes' rest m (i+1))
                else posicoes' rest m (i+1)
    }

main : [Int]
main = posicoes ([1,3,6,2,5,15,3,5,7,18]) 3
-- result = [1,2,5,6,9]