import List

main : Bool
main =
    let list = [1, 2, 3, 4] in
    -- elem
    not (elem 0 []) &&
    not (elem 0 list) &&
    elem 2 list &&
    -- notElem
    notElem 0 [] &&
    notElem 0 list &&
    not (notElem 2 list) &&
    -- filter
    equal (filter (>0) []) [] &&
    equal (filter (<0) list) [] &&
    equal (filter (>0) list) list &&
    equal (filter (\x:Int -> mod x 2 == 0) list) [2, 4] &&
    -- partition 
    equalPair (partition (>0) []) ([], []) &&
    equalPair (partition (<0) list) ([], list) &&
    equalPair (partition (>0) list) ([1, 2, 3, 4], []) &&
    equalPair (partition (\x:Int -> mod x 2 == 0) list) ([2, 4], [1, 3])

equalPair : ([Int], [Int]) -> ([Int], [Int]) -> Bool
equalPair p1 p2 = 
    let (xs , ys ) = p1 in
    let (xs', ys') = p2 in
    equal xs xs' && equal ys ys'
