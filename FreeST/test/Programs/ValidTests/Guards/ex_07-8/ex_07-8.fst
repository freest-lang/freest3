-- VII exercise 8

data Tree = Leaf | Node Char Tree Tree
data Str = Nil | S Char Str

show' : Tree -> Str
show' tree = show'' 1 tree

show'' : Int -> Tree -> Str
show'' s Leaf           = S 'E' (S 'n' Nil)
show'' s (Node c t1 t2) = S c (S ('n') 
                                (str_concat 
                                    (spaces s (show'' (s+1) t1)) 
                                    (spaces s (show'' (s+1) t2))))

str_concat : Str -> Str -> Str
str_concat Nil        s2 = s2
str_concat (S x rest) s2 = S x (str_concat rest s2)

spaces : Int -> Str -> Str
spaces n s 
    | n <= 0    = s 
    | otherwise = S ' ' (spaces (n-1) s)

main : Str
main =
    let t = Node 'p' (Node 'c' Leaf (Node 'g' Leaf Leaf)) (Node 'p' Leaf Leaf) in
    show' t
--result = S 'p' (S 'n' (S ' ' (S 'c' (S 'n' (S ' ' (S ' ' (S 'E' (S 'n' (S ' ' (S ' ' (S 'g' (S 'n' (S ' ' (S ' ' (S ' ' (S 'E' (S 'n' (S ' ' (S ' ' (S ' ' (S 'E' (S 'n' (S ' ' (S 'p' (S 'n' (S ' ' (S ' ' (S 'E' (S 'n' (S ' ' (S ' ' (S 'E' (S 'n' Nil)))))))))))))))))))))))))))))))))
