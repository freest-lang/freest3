import List

main : Bool
main = 
    let list = [1, 2, 3, 4] in
    -- creation
    equal (append [] (append [1] (append [2] [3, 4]))) list &&
    equal (singleton 1) [1] &&
    -- -- list sections
    head list == 1 && 
    last list == 4 &&
    equal (tail list) [2, 3, 4] &&
    equal (init list) [1, 2, 3] &&
    -- -- other
    null [] &&
    not (null [1]) &&
    length [] == 0 &&
    length [1] == 1
