import List

enumFrom1 : Int -> [Int]
enumFrom1 x 
    | x < 1     = []
    | otherwise = enumFrom1 (x-1) ++ [x]

main : Bool
main =
    let list = [1, 2, 3, 4] in
    -- foldl
    foldl @Int (+) 0 [] == 0 &&
    foldl @Int (+) 0 list /= 0 &&
    -- foldr
    foldr @Int (+) 0 [] == 0 &&
    foldr @Int (+) 0 list /= 0 &&
    
    -- special folds
    -- any
    not (any (>0) []) &&
    any (>0) list &&
    not (any (<0) list) &&
    -- all
    all (>0) [] &&
    all (>0) list &&
    not (all (<0) list) &&
    -- concatMap
    equal (concatMap enumFrom1 []) [] &&
    equal (concatMap enumFrom1 list) [1, 1, 2, 1, 2, 3, 1, 2, 3, 4] &&
    -- sum
    sum [] == 0 &&
    sum list == 10 &&
    -- product
    product [] == 1 &&
    product list == 24 &&
    -- maximum
    maximum list == 4 &&
    -- minimum
    minimum list == 1

