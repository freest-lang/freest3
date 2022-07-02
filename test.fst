
data List = Nil | List Int List

f : List -> List -> Bool
f Nil          Nil = False
f ys           zs  = True
f (Cons x xs)  zs  = True


case 
  Nil 
    case 
      Nil False
      Cons True
  Cons x xs 
    case 
      Nil = True
main : Int
main = 1
