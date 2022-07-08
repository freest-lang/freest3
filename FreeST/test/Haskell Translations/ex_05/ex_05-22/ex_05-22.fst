-- V exercise 22

data IntList = End | List Int IntList

selectApply : (Int -> Int) -> (Int -> Bool) -> IntList -> IntList
selectApply applier iffer list =
    case list of {
        End -> End,
        List x rest ->
            if iffer x
                then List (applier x) (selectApply applier iffer rest)
                else (selectApply applier iffer rest)
    }

list : IntList
list = List (-4) (List (-3) (List (-2) (List (-1) 
       (List 0 (List 1 (List 2 (List 3 (List 4 End))))))))

main : IntList
main = selectApply (\x : Int -> x*3) (\x : Int -> x > 0) list
--result = List 3 (List 6 (List 9 (List 12 End)))
