
-- It doesn't really matter, it should not throw an error here
main : Int
main = 23

data T = T1 | T2 

fun : Int -> Int
fun c =
   case c of
    T1 -> 23
    T2 -> 32
