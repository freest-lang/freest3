-- V exercise 22

selectApply : (Int -> Int) -> (Int -> Bool) -> [Int] -> [Int]
selectApply applier iffer list =
    case list of {
        [] -> [],
        x :: rest ->
            if iffer x
                then (applier x) :: (selectApply applier iffer rest)
                else (selectApply applier iffer rest)
    }

list : [Int]
list = [-4,-3,-2,-1,0,1,2,3,4]

main : [Int]
main = selectApply (\x : Int -> x*3) (\x : Int -> x > 0) list
--result = [3,6,9,12]