-- V exercise 25

data IntList = E | L Int IntList
data IntListList = End | List IntList IntListList

after : (IntListList -> IntList) ->  (IntList -> IntListList) -> (IntList -> IntList) 
after f1 f2 = (\list : IntList -> f1 (f2 list))

concat' : IntListList -> IntList
concat' list =
    case list of {
        End -> E,
        List x rest -> merge x (concat' rest)
    }

merge : IntList -> IntList -> IntList
merge a b =
    case a of {
        E -> b,
        L x rest -> L x (merge rest b)
    }

map' : (Int -> IntList) -> IntList -> IntListList
map' f list =
    case list of {
        E -> End,
        L x rest -> List (f x) (map' f rest)
    }

{-            This does not work
filter' : (Int -> Bool) -> IntList -> IntList
filter' f l = 
    let box = (\x:Int -> if f x then L x E else E) in
    after concat' (map' box) l
-}

--              This does work
filter' : (Int -> Bool) -> IntList -> IntList
filter' f l = 
    after concat' (map' (\x:Int -> if f x then L x E else E)) l

list : IntList
list = L (-2) (L (-1) (L 0 (L 1 (L 2 E))))

main : IntList
main = filter' (\x : Int -> x > 0) list
--result = List 1 (List 2 End)