-- import List

main : Bool
main = 
    let list = [1, 2, 3, 4] in
    equal (map (+1) list) [2, 3, 4, 5] &&
    equal (reverse list) [4, 3, 2, 1] &&
    equal (intersperse 0 []) [] &&
    equal (intersperse 0 [1]) [1] &&
    equal (intersperse 0 list) [1, 0, 2, 0, 3, 0, 4]