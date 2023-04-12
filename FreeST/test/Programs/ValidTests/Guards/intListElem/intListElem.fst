data IntList = Nil | Cons Int IntList

elem' : Int -> IntList -> Bool
elem' a Nil = False
elem' a (Cons x y) 
  | a == x    = True 
  | otherwise = elem' a y

main : Bool
main = elem' 23 (Cons 5 (Cons 7 Nil))
