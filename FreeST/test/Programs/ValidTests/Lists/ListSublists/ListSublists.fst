import List

main : Bool
main =
    let list = [1, 2, 3, 4] in
    -- take
    equal (take 1 []) [] &&
    equal (take 0 list) [] &&
    equal (take 2 list) [1, 2] &&
    equal (take 4 list) list &&
    equal (take 5 list) list &&
    -- drop 
    equal (drop 1 []) [] &&
    equal (drop 0 list) list &&
    equal (drop 2 list) [3, 4] &&
    equal (drop 4 list) [] &&
    equal (drop 5 list) [] &&
    -- splitAt
    equalPair (splitAt 2 []) ([], []) &&
    equalPair (splitAt 0 list) ([], list) &&
    equalPair (splitAt 2 list) ([1, 2], [3, 4]) &&
    equalPair (splitAt 4 list) (list, []) &&
    equalPair (splitAt 5 list) (list, []) &&
    -- takeWhile
    equal (takeWhile (>0) []) [] &&
    equal (takeWhile (>0) list) list &&
    equal (takeWhile (<0) list) [] &&
    equal (takeWhile (<3) list) [1, 2] &&
    -- dropWhile
    equal (dropWhile (>0) []) [] &&
    equal (dropWhile (>0) list) [] &&
    equal (dropWhile (<0) list) list &&
    equal (dropWhile (<3) list) [3, 4] &&
    -- span
    equalPair (span (>0) []) ([], []) &&
    equalPair (span (>0) list) (list, []) &&
    equalPair (span (<0) list) ([], list) &&
    equalPair (span (<3) list) ([1, 2], [3, 4]) &&
    -- break
    equalPair (break (>0) []) ([], []) &&
    equalPair (break (<0) list) (list, []) &&
    equalPair (break (>0) list) ([], list) &&
    equalPair (break (>=3) list) ([1, 2], [3, 4])

equalPair : ([Int], [Int]) -> ([Int], [Int]) -> Bool
equalPair p1 p2 = 
    let (xs , ys ) = p1 in
    let (xs', ys') = p2 in
    equal xs xs' && equal ys ys'