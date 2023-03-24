import List

main : Bool
main =
    let list = [1, 2, 3, 4] in
    -- scanl
    equal (scanl (+) 0 []) [0] &&
    equal (scanl (+) 0 list) [0, 1, 3, 6, 10] &&
    -- scanl1
    equal (scanl1 (+) []) [] &&
    equal (scanl1 (+) [1]) [1] &&
    equal (scanl1 (+) list) [1, 3, 6, 10] &&
    -- scanr
    equal (scanr (+) 0 []) [0] &&
    equal (scanr (+) 0 list) [10,9,7,4,0] && -- [0, 1, 3, 6, 10]
    -- scanr1
    equal (scanr1 (+) []) [] &&
    equal (scanr1 (+) [1]) [1] &&
    equal (scanr1 (+) list) [10,9,7,4] -- [1, 3, 6, 10]
